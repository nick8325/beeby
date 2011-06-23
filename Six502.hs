-- A 6502 emulator, parametrised on the backend
-- (the thing that actually does computations and I/O).
-- The point of this parametrisation is so that we can
-- use the same code for interpreting and JITting.

{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
module Six502 where

import Control.Monad

-- 6502 documentation from
-- http://www.obelisk.demon.co.uk/6502/
-- Flags register information from
-- http://www.atarimagazines.com/compute/issue53/047_1_All_About_The_Status_Register.php
-- Documentation on decimal mode from
-- http://www.6502.org/tutorials/decimal_mode.html
-- BRK instruction:
-- http://nesdev.parodius.com/the%20'B'%20flag%20&%20BRK%20instruction.txt

data Flag = Carry | Zero | InterruptDisable | Decimal | Overflow | Negative
data Register = A | X | Y | Stack

data Location m =
  Location { peek :: m (Byte m),
             poke :: Byte m -> m () }

class Monad m => Machine m where
  data Addr m
  data Byte m
  data Bit m
  
  address :: Int -> Addr m
  index :: Addr m -> Byte m -> Addr m
  page :: Addr m -> Byte m
  offset :: Addr m -> Byte m
  paged :: Byte m -> Byte m -> Addr m
  byte :: Int -> Byte m
  bit :: Bool -> Bit m

  shl :: Byte m -> Byte m
  shr :: Byte m -> Byte m
  selectBit :: Int -> Byte m -> Bit m
  oneBit :: Int -> Bit m -> Byte m
  zero :: Byte m -> Bit m
  eq :: Byte m -> Byte m -> Bit m
  geq :: Byte m -> Byte m -> Bit m

  add :: Byte m -> Byte m -> Byte m
  carry :: Byte m -> Byte m -> Bit m
  toBCD :: Byte m -> Byte m
  fromBCD :: Byte m -> Byte m
  
  and_ :: Byte m -> Byte m -> Byte m
  or_ :: Byte m -> Byte m -> Byte m
  xor :: Byte m -> Byte m -> Byte m
  bitOr :: Bit m -> Bit m -> Bit m

  memory :: Addr m -> Location m
  register :: Register -> Location m
  flag :: Flag -> m (Bit m)
  setFlag :: Flag -> Bit m -> m ()
  loadPC :: m (Addr m)
  storePC :: Addr m -> m ()

  cond :: Bit m -> m a -> m a -> m a

  fetch :: m Int
  tick :: Int -> m ()

{-# INLINE zeroPage #-}
zeroPage :: Machine m => Byte m -> Addr m
zeroPage = paged (byte 0)

{-# INLINE stackPage #-}
stackPage :: Machine m => Byte m -> Addr m
stackPage = paged (byte 1)

{-# INLINE push #-}
push :: Machine m => Byte m -> m ()
push x = do
  addr <- peek (register Stack)
  poke (register Stack) (add addr (byte (-1)))
  poke (memory (stackPage addr)) x

{-# INLINE pop #-}
pop :: Machine m => m (Byte m)
pop = do
  addr <- peek (register Stack)
  poke (register Stack) (add addr (byte 1))
  peek (memory (stackPage (add addr (byte 1))))

{-# INLINE push16 #-}
push16 :: Machine m => Addr m -> m ()
push16 addr = do
  push (page addr)
  push (offset addr)

{-# INLINE pop16 #-}
pop16 :: Machine m => m (Addr m)
pop16 = do
  o <- pop
  p <- pop
  return (paged p o)

{-# INLINE peek16 #-}
peek16 :: Machine m => Addr m -> m (Addr m)
peek16 addr = do
  x <- peek (memory addr)
  y <- peek (memory (addr `index` byte 1))
  return (paged y x)

{-# INLINE imm #-}
imm :: Machine m => m (Byte m)
imm = liftM byte fetch

{-# INLINE zp #-}
zp :: Machine m => m (Addr m)
zp = liftM (zeroPage . byte) fetch

{-# INLINE zpRel #-}
zpRel :: Machine m => Register -> m (Addr m)
zpRel r = do
  x <- fetch
  y <- peek (register r)
  return (zeroPage (add (byte x) y))

{-# INLINE absolute #-}
absolute :: Machine m => m (Addr m)
absolute = do
  x <- imm
  y <- imm
  return (paged y x)

{-# INLINE indexed #-}
indexed :: Machine m => Register -> m (Addr m)
indexed r = do
  addr <- absolute
  x <- peek (register r)
  return (addr `index` x)

{-# INLINE relative #-}
relative :: Machine m => m (Addr m)
relative = do
  -- important to do imm before loadPC because address
  -- should be relative to *final* value of pc
  x <- imm
  pc <- loadPC
  return (pc `index` x)

{-# INLINE indirect #-}
indirect :: Machine m => m (Addr m)
indirect = do
  addr <- absolute
  addr' <- peek16 addr
  return addr'

{-# INLINE indirectX #-}
indirectX :: Machine m => m (Addr m)
indirectX = do
  x <- fetch
  y <- peek (register X)
  addr <- peek16 (zeroPage (add (byte x) y))
  return addr

{-# INLINE indirectY #-}
indirectY :: Machine m => m (Addr m)
indirectY = do
  x <- fetch
  addr <- peek16 (zeroPage (byte x))
  y <- peek (register Y)
  return (addr `index` y)

{-# INLINE cpu #-}
cpu :: Machine m => m ()
cpu = fetch >>= decode
  where -- LDA
        decode 0xa9 = do { tick 2; imm >>= ld A }
        decode 0xa5 = do { tick 3; zp >>= ldaMem }
        decode 0xb5 = do { tick 4; zpRel X >>= ldaMem }
        decode 0xad = do { tick 4; absolute >>= ldaMem }
        decode 0xbd = do { tick 4; indexed X >>= ldaMem }
        decode 0xb9 = do { tick 4; indexed Y >>= ldaMem }
        decode 0xa1 = do { tick 6; indirectX >>= ldaMem }
        decode 0xb1 = do { tick 5; indirectY >>= ldaMem }
        -- LDX
        decode 0xa2 = do { tick 2; imm >>= ld X }
        decode 0xa6 = do { tick 3; zp >>= ldxMem }
        decode 0xb6 = do { tick 4; zpRel Y >>= ldxMem }
        decode 0xae = do { tick 4; absolute >>= ldxMem }
        decode 0xbe = do { tick 4; indexed Y >>= ldxMem }
        -- LDY
        decode 0xa0 = do { tick 2; imm >>= ld Y }
        decode 0xa4 = do { tick 3; zp >>= ldyMem }
        decode 0xb4 = do { tick 4; zpRel X >>= ldyMem }
        decode 0xac = do { tick 4; absolute >>= ldyMem }
        decode 0xbc = do { tick 4; indexed X >>= ldyMem }
        -- STA
        decode 0x85 = do { tick 3; zp >>= sta }
        decode 0x95 = do { tick 4; zpRel X >>= sta }
        decode 0x8d = do { tick 4; absolute >>= sta }
        decode 0x9d = do { tick 5; indexed X >>= sta }
        decode 0x99 = do { tick 5; indexed Y >>= sta }
        decode 0x81 = do { tick 6; indirectX >>= sta }
        decode 0x91 = do { tick 6; indirectY >>= sta }
        -- STX
        decode 0x86 = do { tick 3; zp >>= stx }
        decode 0x96 = do { tick 4; zpRel Y >>= stx }
        decode 0x8e = do { tick 4; absolute >>= stx }
        -- STY
        decode 0x84 = do { tick 3; zp >>= sty }
        decode 0x94 = do { tick 4; zpRel X >>= sty }
        decode 0x8c = do { tick 4; absolute >>= sty }
        -- TAX
        decode 0xaa = do { tick 2; transfer A X }
        -- TAY
        decode 0xa8 = do { tick 2; transfer A Y }
        -- TXA
        decode 0x8a = do { tick 2; transfer X A }
        -- TYA
        decode 0x98 = do { tick 2; transfer Y A }
        -- TSX
        decode 0xba = do { tick 2; transfer Stack X }
        -- TXS
        decode 0x9a = do
          tick 2
          x <- peek (register X)
          poke (register Stack) x
        -- PHA
        decode 0x48 = do
          tick 3
          x <- peek (register A)
          push x
        -- PLA
        decode 0x68 = do
          tick 4
          x <- pop
          poke (register A) x
          zeroNeg x
        -- PHP
        decode 0x08 = do { tick 3; php (bit False) }
        -- PLP
        decode 0x28 = do { tick 2; plp }
        -- AND
        decode 0x29 = do { tick 2; imm >>= acc and_ }
        decode 0x25 = do { tick 3; zp >>= andMem }
        decode 0x35 = do { tick 4; zpRel X >>= andMem }
        decode 0x2d = do { tick 4; absolute >>= andMem }
        decode 0x3d = do { tick 4; indexed X >>= andMem }
        decode 0x39 = do { tick 4; indexed Y >>= andMem }
        decode 0x21 = do { tick 6; indirectX >>= andMem }
        decode 0x31 = do { tick 5; indirectY >>= andMem }
        -- EOR
        decode 0x49 = do { tick 2; imm >>= acc xor }
        decode 0x45 = do { tick 3; zp >>= xorMem }
        decode 0x55 = do { tick 4; zpRel X >>= xorMem }
        decode 0x4d = do { tick 4; absolute >>= xorMem }
        decode 0x5d = do { tick 4; indexed X >>= xorMem }
        decode 0x59 = do { tick 4; indexed Y >>= xorMem }
        decode 0x41 = do { tick 6; indirectX >>= xorMem }
        decode 0x51 = do { tick 5; indirectY >>= xorMem }
        -- ORA
        decode 0x09 = do { tick 2; imm >>= acc or_ }
        decode 0x05 = do { tick 3; zp >>= orMem }
        decode 0x15 = do { tick 4; zpRel X >>= orMem }
        decode 0x0d = do { tick 4; absolute >>= orMem }
        decode 0x1d = do { tick 4; indexed X >>= orMem }
        decode 0x19 = do { tick 4; indexed Y >>= orMem }
        decode 0x01 = do { tick 6; indirectX >>= orMem }
        decode 0x11 = do { tick 5; indirectY >>= orMem }
        -- BIT
        decode 0x24 = do { tick 3; zp >>= bitTest }
        decode 0x2c = do { tick 4; absolute >>= bitTest }
        -- ADC
        decode 0x69 = do { tick 2; imm >>= adc }
        decode 0x65 = do { tick 3; zp >>= adcMem }
        decode 0x75 = do { tick 4; zpRel X >>= adcMem }
        decode 0x6d = do { tick 4; absolute >>= adcMem }
        decode 0x7d = do { tick 4; indexed X >>= adcMem }
        decode 0x79 = do { tick 4; indexed Y >>= adcMem }
        decode 0x61 = do { tick 6; indirectX >>= adcMem }
        decode 0x71 = do { tick 5; indirectY >>= adcMem }
        -- SBC
        decode 0xe9 = do { tick 2; imm >>= sbc }
        decode 0xe5 = do { tick 3; zp >>= sbcMem }
        decode 0xf5 = do { tick 4; zpRel X >>= sbcMem }
        decode 0xed = do { tick 4; absolute >>= sbcMem }
        decode 0xfd = do { tick 4; indexed X >>= sbcMem }
        decode 0xf9 = do { tick 4; indexed Y >>= sbcMem }
        decode 0xe1 = do { tick 6; indirectX >>= sbcMem }
        decode 0xf1 = do { tick 5; indirectY >>= sbcMem }
        -- CMP
        decode 0xc9 = do { tick 2; imm >>= cmp A }
        decode 0xc5 = do { tick 3; zp >>= (cmpMem A) }
        decode 0xd5 = do { tick 4; zpRel X >>= (cmpMem A) }
        decode 0xcd = do { tick 4; absolute >>= (cmpMem A) }
        decode 0xdd = do { tick 4; indexed X >>= (cmpMem A) }
        decode 0xd9 = do { tick 4; indexed Y >>= (cmpMem A) }
        decode 0xc1 = do { tick 6; indirectX >>= (cmpMem A) }
        decode 0xd1 = do { tick 5; indirectY >>= (cmpMem A) }
        -- CPX
        decode 0xe0 = do { tick 2; imm >>= cmp X }
        decode 0xe4 = do { tick 3; zp >>= (cmpMem X) }
        decode 0xec = do { tick 4; absolute >>= (cmpMem X) }
        -- CPY
        decode 0xc0 = do { tick 2; imm >>= cmp Y }
        decode 0xc4 = do { tick 3; zp >>= (cmpMem Y) }
        decode 0xcc = do { tick 4; absolute >>= (cmpMem Y) }
        -- INC
        decode 0xe6 = do { tick 5; zp >>= incMem }
        decode 0xf6 = do { tick 6; zpRel X >>= incMem }
        decode 0xee = do { tick 6; absolute >>= incMem }
        decode 0xfe = do { tick 7; indexed X >>= incMem }
        -- INX
        decode 0xe8 = do { tick 2; inc (register X) }
        -- INY
        decode 0xc8 = do { tick 2; inc (register Y) }
        -- DEC
        decode 0xc6 = do { tick 5; zp >>= decMem }
        decode 0xd6 = do { tick 6; zpRel X >>= decMem }
        decode 0xce = do { tick 6; absolute >>= decMem }
        decode 0xde = do { tick 7; indexed X >>= decMem }
        -- DEX
        decode 0xca = do { tick 2; dec (register X) }
        -- DEY
        decode 0x88 = do { tick 2; dec (register Y) }
        -- ASL
        decode 0x0a = do { tick 2; asl (register A) }
        decode 0x06 = do { tick 5; zp >>= aslMem }
        decode 0x16 = do { tick 6; zpRel X >>= aslMem }
        decode 0x0e = do { tick 6; absolute >>= aslMem }
        decode 0x1e = do { tick 7; indexed X >>= aslMem }
        -- LSR
        decode 0x4a = do { tick 2; lsr (register A) }
        decode 0x46 = do { tick 5; zp >>= lsrMem }
        decode 0x56 = do { tick 6; zpRel X >>= lsrMem }
        decode 0x4e = do { tick 6; absolute >>= lsrMem }
        decode 0x5e = do { tick 7; indexed X >>= lsrMem }
        -- ROL
        decode 0x2a = do { tick 2; rol (register A) }
        decode 0x26 = do { tick 5; zp >>= rolMem }
        decode 0x36 = do { tick 6; zpRel X >>= rolMem }
        decode 0x2e = do { tick 6; absolute >>= rolMem }
        decode 0x3e = do { tick 7; indexed X >>= rolMem }
        -- ROR
        decode 0x6a = do { tick 2; ror (register A) }
        decode 0x66 = do { tick 5; zp >>= rorMem }
        decode 0x76 = do { tick 6; zpRel X >>= rorMem }
        decode 0x6e = do { tick 6; absolute >>= rorMem }
        decode 0x7e = do { tick 7; indexed X >>= rorMem }
        -- JMP
        decode 0x4c = do { tick 3; absolute >>= storePC }
        decode 0x6c = do { tick 5; indirect >>= storePC }
        -- JSR
        decode 0x20 = do { tick 6; absolute >>= jsr }
        -- RTS
        decode 0x60 = do { tick 6; rts }
        -- branch instructions
        decode 0x90 = do { tick 2; branchNot Carry }
        decode 0xb0 = do { tick 2; branchIf Carry }
        decode 0xd0 = do { tick 2; branchNot Zero }
        decode 0xf0 = do { tick 2; branchIf Zero }
        decode 0x10 = do { tick 2; branchNot Negative }
        decode 0x30 = do { tick 2; branchIf Negative }
        decode 0x50 = do { tick 2; branchNot Overflow }
        decode 0x70 = do { tick 2; branchIf Overflow }
        -- flag instructions
        decode 0x18 = do { tick 2; setFlag Carry (bit False) }
        decode 0xd8 = do { tick 2; setFlag Decimal (bit False) }
        decode 0x58 = do { tick 2; setFlag InterruptDisable (bit False) }
        decode 0xb8 = do { tick 2; setFlag Overflow (bit False) }
        decode 0x38 = do { tick 2; setFlag Carry (bit True) }
        decode 0xf8 = do { tick 2; setFlag Decimal (bit True) }
        decode 0x78 = do { tick 2; setFlag InterruptDisable (bit True) }
        -- BRK
        decode 0x00 = do { tick 7; brk }
        -- NOP
        decode 0xea = do { tick 2 }
        -- RTI
        decode 0x40 = do { tick 6; rti }
        -- unknown
        decode x = error $ "unknown opcode " ++ show x

        -- We mark absolutely every higher-order function for inlining,
        -- so as to straighten the control flow out.
        -- This includes functions that are parametrised on a register
        -- or a flag, because they have totally different control flow depending
        -- on which register or flag we choose.
        -- However, we often don't annotate first-order functions with
        -- INLINE. We refuse to inline the BCD routines because
        -- people with good taste wouldn't use them.

        ldaMem = ldMem A
        ldxMem = ldMem X
        ldyMem = ldMem Y
        {-# INLINE ldMem #-}
        ldMem r addr = peek (memory addr) >>= ld r
        {-# INLINE ld #-}
        ld r v = do
          poke (register r) v
          zeroNeg v

        sta = st A
        stx = st X
        sty = st Y

        {-# INLINE st #-}
        st r addr = do
          x <- peek (register r)
          poke (memory addr) x
          
        {-# INLINE transfer #-}
        transfer r1 r2 = do
          x <- peek (register r1)
          poke (register r2) x
          zeroNeg x

        php break = do
          b7 <- flag Negative
          b6 <- flag Overflow
          b3 <- flag Decimal
          b2 <- flag InterruptDisable
          b1 <- flag Zero
          b0 <- flag Carry
          push (oneBit 0 b0 `add`
                oneBit 1 b1 `add`
                oneBit 2 b2 `add`
                oneBit 3 b3 `add`
                oneBit 4 break `add`
                oneBit 5 (bit True) `add`
                oneBit 6 b6 `add`
                oneBit 7 b7)

        {-# INLINE plp #-}
        plp = do
          x <- pop
          setFlag Negative (selectBit 7 x)
          setFlag Overflow (selectBit 6 x)
          setFlag Decimal (selectBit 3 x)
          setFlag InterruptDisable (selectBit 2 x)
          setFlag Zero (selectBit 1 x)
          setFlag Carry (selectBit 0 x)

        andMem = accMem and_
        orMem = accMem or_
        xorMem = accMem xor

        {-# INLINE accMem #-}
        accMem op addr = peek (memory addr) >>= acc op
        {-# INLINE acc #-}
        acc op x = do
          y <- peek (register A)
          poke (register A) (x `op` y)
          zeroNeg (x `op` y)

        bitTest addr = do
          x <- peek (register A)
          y <- peek (memory addr)
          setFlag Zero (x `eq` y)
          setFlag Negative (selectBit 7 y)
          setFlag Overflow (selectBit 6 y)

        adcMem addr = peek (memory addr) >>= adc
        {-# INLINE adc #-}
        adc x = do
          f <- flag Decimal
          cond f (adcBCD x) (adcNormal (register A) x)
          
        {-# NOINLINE adcBCD #-}
        adcBCD x = adcNormal l (fromBCD x)
          where l = Location {
                  peek = liftM fromBCD (peek (register A)),
                  poke = \v -> do
                   poke (register A) (toBCD v)
                   setFlag Carry (geq v (byte 100)) }
          
        {-# INLINE adcNormal #-}
        adcNormal l x = do
          c <- flag Carry
          y <- peek l
          let !z = x `add` y `add` oneBit 0 c
              !zc = (x `carry` y) `bitOr` ((x `add` y) `carry` oneBit 0 c)
              x `equ` y = x `xor` y `xor` byte (-1)
          zeroNeg z
          setFlag Carry zc
          setFlag Overflow (selectBit 7 ((x `equ` y) `xor` z))
          -- important that this goes at the end so that poke can set the carry flag
          -- in BCD mode
          poke l z

        sbcMem addr = peek (memory addr) >>= sbc
        {-# INLINE sbc #-}
        sbc x = do
          f <- flag Decimal
          cond f (sbcBCD x) (adcNormal (register A) (xor x (byte (-1))))

        {-# NOINLINE sbcBCD #-}
        sbcBCD x = adcNormal l (xor (fromBCD x) (byte (-1)))
          where l = Location {
                  peek = liftM fromBCD (peek (register A)),
                  poke = \v -> poke (register A) (toBCD v) }

        {-# INLINE cmpMem #-}
        cmpMem r addr = peek (memory addr) >>= cmp r
        {-# INLINE cmp #-}
        cmp r x = do
          y <- peek (register r)
          setFlag Carry (bit True)
          adcNormal (register r) (xor x (byte (-1)))
          poke (register r) y

        incMem = inc . memory
        decMem = dec . memory

        {-# INLINE inc #-}
        inc = adjust (byte 1)
        {-# INLINE dec #-}
        dec = adjust (byte (-1))
        {-# INLINE adjust #-}
        adjust x l = do
          y <- peek l
          poke l (x `add` y)
          zeroNeg (x `add` y)

        aslMem = asl . memory
        lsrMem = lsr . memory
        rolMem = rol . memory
        rorMem = ror . memory

        {-# INLINE asl #-}
        asl l = rotate l leftRotate (bit False)
        {-# INLINE lsr #-}
        lsr l = rotate l rightRotate (bit False)
        {-# INLINE rol #-}
        rol l = flag Carry >>= rotate l leftRotate
        {-# INLINE ror #-}
        ror l = flag Carry >>= rotate l rightRotate
        
        {-# INLINE rotate #-}
        rotate l f c = do
          x <- peek l
          let !(x', c') = f x c
          poke l x'
          setFlag Carry c'
          zeroNeg x'
        
        {-# INLINE leftRotate #-}
        leftRotate x c = (shl x `add` oneBit 0 c, selectBit 7 x)
        {-# INLINE rightRotate #-}
        rightRotate x c = (shr x `add` oneBit 7 c, selectBit 0 x)

        jsr addr = do
          pc <- loadPC
          push16 (pc `index` byte (-1))
          storePC addr
        
        rts = do
          pc <- pop16
          storePC (pc `index` byte 1)

        {-# INLINE branchIf #-}
        branchIf f = do
          v <- flag f
          addr <- relative
          cond v (storePC addr) (return ())
          
        {-# INLINE branchNot #-}
        branchNot f = do
          v <- flag f
          addr <- relative
          cond v (return ()) (storePC addr)

        brk = do
          pc <- loadPC
          push16 (pc `index` byte (-1))
          php (bit True)
          pc' <- peek16 (address 0xfffe)
          storePC pc'

        rti = do
          plp
          pc <- pop16
          storePC (pc `index` byte 2)

        {-# INLINE zeroNeg #-}
        zeroNeg v = do
          setFlag Zero (zero v)
          setFlag Negative (selectBit 7 v)

{-# INLINE reset #-}
reset ::Machine m => m ()
reset = do
  peek16 (address 0xfffc) >>= storePC
  setFlag InterruptDisable (bit True)
  setFlag Decimal (bit False)
  poke (register Stack) (byte 0xff)

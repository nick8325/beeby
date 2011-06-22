{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
module Six502 where

-- 6502 documentation from
-- http://www.obelisk.demon.co.uk/6502/
-- Flags register information from
-- http://www.atarimagazines.com/compute/issue53/047_1_All_About_The_Status_Register.php
-- Documentation on decimal mode from
-- http://www.6502.org/tutorials/decimal_mode.html
-- BRK instruction:
-- http://nesdev.parodius.com/the%20'B'%20flag%20&%20BRK%20instruction.txt
import Data.Word

data Flag = Carry | Zero | InterruptDisable | Decimal | Overflow | Negative
data Register = A | X | Y | Stack

data Location program =
  Location { peek :: (Byte program -> program) -> program,
             poke :: Byte program -> program -> program }

class Program program where
  data Addr program
  data Byte program
  data Bit program
  
  {-# INLINE address #-}
  address :: Word16 -> Addr program
  {-# INLINE index #-}
  index :: Addr program -> Byte program -> Addr program
  {-# INLINE page #-}
  page :: Addr program -> Byte program
  {-# INLINE offset #-}
  offset :: Addr program -> Byte program
  {-# INLINE paged #-}
  paged :: Byte program -> Byte program -> Addr program
  {-# INLINE byte #-}
  byte :: Word8 -> Byte program
  {-# INLINE bit #-}
  bit :: Bool -> Bit program

  {-# INLINE shl #-}
  shl :: Byte program -> Bit program -> Byte program
  {-# INLINE shr #-}
  shr :: Byte program -> Bit program -> Byte program
  {-# INLINE selectBit #-}
  selectBit :: Int -> Byte program -> Bit program
  {-# INLINE oneBit #-}
  oneBit :: Int -> Bit program -> Byte program
  {-# INLINE zero #-}
  zero :: Byte program -> Bit program
  {-# INLINE geq #-}
  geq :: Byte program -> Byte program -> Bit program

  {-# INLINE add #-}
  add :: Byte program -> Byte program -> Byte program
  {-# INLINE carry #-}
  carry :: Byte program -> Byte program -> Bit program
  {-# INLINE toBCD #-}
  toBCD :: Byte program -> Byte program
  {-# INLINE fromBCD #-}
  fromBCD :: Byte program -> Byte program
  
  {-# INLINE and_ #-}
  and_ :: Byte program -> Byte program -> Byte program
  {-# INLINE or_ #-}
  or_ :: Byte program -> Byte program -> Byte program
  {-# INLINE xor #-}
  xor :: Byte program -> Byte program -> Byte program

  {-# INLINE memory #-}
  memory :: Addr program -> Location program
  {-# INLINE register #-}
  register :: Register -> Location program
  {-# INLINE flag #-}
  flag :: Flag -> (Bit program -> program) -> program
  {-# INLINE setFlag #-}
  setFlag :: Flag -> Bit program -> program -> program
  {-# INLINE loadPC #-}
  loadPC :: (Addr program -> program) -> program
  {-# INLINE storePC #-}
  storePC :: Addr program -> program -> program

  {-# INLINE cond #-}
  cond :: Bit program -> program -> program -> program

  {-# INLINE fetch #-}
  fetch :: (Word8 -> program) -> program
  {-# INLINE tick #-}
  tick :: Int -> program -> program

  {-# INLINE done #-}
  done :: program

{-# INLINE bitOp #-}
bitOp :: Program program =>
         (Byte program -> Byte program -> Byte program) ->
         Bit program -> Bit program -> Bit program
bitOp op x y = selectBit 0 (oneBit 0 x `op` oneBit 0 y)

{-# INLINE push #-}
push :: Program program => Byte program -> program -> program
push x k =
  peek (register Stack) $ \addr ->
  poke (register Stack) (add addr (byte (-1))) $
  poke (memory (zeroPage addr)) x $
  k

{-# INLINE pop #-}
pop :: Program program => (Byte program -> program) -> program
pop k =
  peek (register Stack) $ \addr ->
  poke (register Stack) (add addr (byte 1)) $
  peek (memory (zeroPage addr)) $ \x ->
  k x

{-# INLINE push16 #-}
push16 :: Program program => Addr program -> program -> program
push16 addr k =
  push (page addr) $
  push (offset addr) $
  k

{-# INLINE pop16 #-}
pop16 :: Program program => (Addr program -> program) -> program
pop16 k =
  pop $ \o ->
  pop $ \p ->
  k (paged p o)

{-# INLINE peek16 #-}
peek16 :: Program program => Addr program -> (Addr program -> program) -> program
peek16 addr k =
  peek (memory addr) $ \x ->
  peek (memory (addr `index` byte 1)) $ \y ->
  k (paged y x)

{-# INLINE imm #-}
imm :: Program program => (Byte program -> program) -> program
imm k = fetch (k . byte)

{-# INLINE zeroPage #-}
zeroPage :: Program program => Byte program -> Addr program
zeroPage = paged (byte 0)

{-# INLINE zp #-}
zp :: Program program => (Addr program -> program) -> program
zp k = fetch (\x -> k (zeroPage (byte x)))

{-# INLINE zpRel #-}
zpRel :: Program program => Register -> (Addr program -> program) -> program
zpRel r k =
  fetch $ \x ->
  peek (register r) $ \y ->
  k (zeroPage (add (byte x) y))

{-# INLINE absolute #-}
absolute :: Program program => (Addr program -> program) -> program
absolute k = fetch $ \x -> fetch $ \y -> k (paged (byte y) (byte x))

{-# INLINE indexed #-}
indexed :: Program program => Register -> (Addr program -> program) -> program
indexed r k =
  absolute $ \addr ->
  peek (register r) $ \x ->
  k (addr `index` x)

{-# INLINE relative #-}
relative :: Program program => (Addr program -> program) -> program
relative k =
  -- important to do imm before loadPC because address
  -- should be relative to *final* value of pc
  imm $ \x ->
  loadPC $ \pc ->
  k (pc `index` x)

{-# INLINE indirect #-}
indirect :: Program program => (Addr program -> program) -> program
indirect k =
  absolute $ \addr ->
  peek16 addr $ \addr' ->
  k addr'

{-# INLINE indirectX #-}
indirectX :: Program program => (Addr program -> program) -> program
indirectX k =
  fetch $ \x ->
  peek (register X) $ \y ->
  peek16 (zeroPage (add (byte x) y)) $ \addr ->
  k addr

{-# INLINE indirectY #-}
indirectY :: Program program => (Addr program -> program) -> program
indirectY k =
  fetch $ \x ->
  peek16 (zeroPage (byte x)) $ \addr ->
  peek (register Y) $ \y ->
  k (index addr y)

cpu :: Program program => program
cpu = fetch decode
  where -- LDA
        decode 0xa9 = tick 2 $ imm (ld A)
        decode 0xa5 = tick 3 $ zp ldaMem
        decode 0xb5 = tick 4 $ zpRel X ldaMem
        decode 0xad = tick 4 $ absolute ldaMem
        decode 0xbd = tick 4 $ indexed X ldaMem
        decode 0xb9 = tick 4 $ indexed Y ldaMem
        decode 0xa1 = tick 6 $ indirectX ldaMem
        decode 0xb1 = tick 5 $ indirectY ldaMem
        -- LDX
        decode 0xa2 = tick 2 $ imm (ld X)
        decode 0xa6 = tick 3 $ zp ldxMem
        decode 0xb6 = tick 4 $ zpRel Y ldxMem
        decode 0xae = tick 4 $ absolute ldxMem
        decode 0xbe = tick 4 $ indexed Y ldxMem
        -- LDY
        decode 0xa0 = tick 2 $ imm (ld Y)
        decode 0xa4 = tick 3 $ zp ldyMem
        decode 0xb4 = tick 4 $ zpRel X ldyMem
        decode 0xac = tick 4 $ absolute ldyMem
        decode 0xbc = tick 4 $ indexed X ldyMem
        -- STA
        decode 0x85 = tick 3 $ zp sta
        decode 0x95 = tick 4 $ zpRel X sta
        decode 0x8d = tick 4 $ absolute sta
        decode 0x9d = tick 5 $ indexed X sta
        decode 0x99 = tick 5 $ indexed Y sta
        decode 0x81 = tick 6 $ indirectX sta
        decode 0x91 = tick 6 $ indirectY sta
        -- STX
        decode 0x86 = tick 3 $ zp stx
        decode 0x96 = tick 4 $ zpRel Y stx
        decode 0x8e = tick 4 $ absolute stx
        -- STY
        decode 0x84 = tick 3 $ zp sty
        decode 0x94 = tick 4 $ zpRel X sty
        decode 0x8c = tick 4 $ absolute sty
        -- TAX
        decode 0xaa = tick 2 $ transfer A X
        -- TAY
        decode 0xa8 = tick 2 $ transfer A Y
        -- TXA
        decode 0x8a = tick 2 $ transfer X A
        -- TYA
        decode 0x98 = tick 2 $ transfer Y A
        -- TSX
        decode 0xba = tick 2 (transfer Stack X)
        -- TXS
        decode 0x9a =
          tick 2 $
          peek (register X) $ \x ->
          poke (register Stack) x $
          done
        -- PHA
        decode 0x48 =
          tick 3 $
          peek (register A) $ \x ->
          push x $
          done
        -- PLA
        decode 0x68 =
          tick 4 $
          pop $ \x ->
          poke (register A) x $
          zeroNeg x $
          done
        -- PHP
        decode 0x08 = tick 3 $ php (bit False) done
        -- PLP
        decode 0x28 = tick 2 $ plp done
        -- AND
        decode 0x29 = tick 2 $ imm (acc and_)
        decode 0x25 = tick 3 $ zp (accMem and_)
        decode 0x35 = tick 4 $ zpRel X (accMem and_)
        decode 0x2d = tick 4 $ absolute (accMem and_)
        decode 0x3d = tick 4 $ indexed X (accMem and_)
        decode 0x39 = tick 4 $ indexed Y (accMem and_)
        decode 0x21 = tick 6 $ indirectX (accMem and_)
        decode 0x31 = tick 5 $ indirectY (accMem and_)
        -- EOR
        decode 0x49 = tick 2 $ imm (acc xor)
        decode 0x45 = tick 3 $ zp (accMem xor)
        decode 0x55 = tick 4 $ zpRel X (accMem xor)
        decode 0x4d = tick 4 $ absolute (accMem xor)
        decode 0x5d = tick 4 $ indexed X (accMem xor)
        decode 0x59 = tick 4 $ indexed Y (accMem xor)
        decode 0x41 = tick 6 $ indirectX (accMem xor)
        decode 0x51 = tick 5 $ indirectY (accMem xor)
        -- ORA
        decode 0x09 = tick 2 $ imm (acc or_)
        decode 0x05 = tick 3 $ zp (accMem or_)
        decode 0x15 = tick 4 $ zpRel X (accMem or_)
        decode 0x0d = tick 4 $ absolute (accMem or_)
        decode 0x1d = tick 4 $ indexed X (accMem or_)
        decode 0x19 = tick 4 $ indexed Y (accMem or_)
        decode 0x01 = tick 6 $ indirectX (accMem or_)
        decode 0x11 = tick 5 $ indirectY (accMem or_)
        -- BIT
        decode 0x24 = tick 3 $ zp bitTest
        decode 0x2c = tick 4 $ absolute bitTest
        -- ADC
        decode 0x69 = tick 2 $ imm adc
        decode 0x65 = tick 3 $ zp adcMem
        decode 0x75 = tick 4 $ zpRel X adcMem
        decode 0x6d = tick 4 $ absolute adcMem
        decode 0x7d = tick 4 $ indexed X adcMem
        decode 0x79 = tick 4 $ indexed Y adcMem
        decode 0x61 = tick 6 $ indirectX adcMem
        decode 0x71 = tick 5 $ indirectY adcMem
        -- SBC
        decode 0xe9 = tick 2 $ imm sbc
        decode 0xe5 = tick 3 $ zp sbcMem
        decode 0xf5 = tick 4 $ zpRel X sbcMem
        decode 0xed = tick 4 $ absolute sbcMem
        decode 0xfd = tick 4 $ indexed X sbcMem
        decode 0xf9 = tick 4 $ indexed Y sbcMem
        decode 0xe1 = tick 6 $ indirectX sbcMem
        decode 0xf1 = tick 5 $ indirectY sbcMem
        -- CMP
        decode 0xc9 = tick 2 $ imm (cmp A)
        decode 0xc5 = tick 3 $ zp (cmpMem A)
        decode 0xd5 = tick 4 $ zpRel X (cmpMem A)
        decode 0xcd = tick 4 $ absolute (cmpMem A)
        decode 0xdd = tick 4 $ indexed X (cmpMem A)
        decode 0xd9 = tick 4 $ indexed Y (cmpMem A)
        decode 0xc1 = tick 6 $ indirectX (cmpMem A)
        decode 0xd1 = tick 5 $ indirectY (cmpMem A)
        -- CPX
        decode 0xe0 = tick 2 $ imm (cmp X)
        decode 0xe4 = tick 3 $ zp (cmpMem X)
        decode 0xec = tick 4 $ absolute (cmpMem X)
        -- CPY
        decode 0xc0 = tick 2 $ imm (cmp Y)
        decode 0xc4 = tick 3 $ zp (cmpMem Y)
        decode 0xcc = tick 4 $ absolute (cmpMem Y)
        -- INC
        decode 0xe6 = tick 5 $ zp (inc . memory)
        decode 0xf6 = tick 6 $ zpRel X (inc . memory)
        decode 0xee = tick 6 $ absolute (inc . memory)
        decode 0xfe = tick 7 $ indexed X (inc . memory)
        -- INX
        decode 0xe8 = tick 2 $ inc (register X)
        -- INY
        decode 0xc8 = tick 2 $ inc (register Y)
        -- DEC
        decode 0xc6 = tick 5 $ zp (dec . memory)
        decode 0xd6 = tick 6 $ zpRel X (dec . memory)
        decode 0xce = tick 6 $ absolute (dec . memory)
        decode 0xde = tick 7 $ absolute (dec . memory)
        -- DEX
        decode 0xca = tick 2 $ dec (register X)
        -- DEY
        decode 0x88 = tick 2 $ dec (register Y)
        -- ASL
        decode 0x0a = tick 2 $ asl (register A)
        decode 0x06 = tick 5 $ zp (asl . memory)
        decode 0x16 = tick 6 $ zpRel X (asl . memory)
        decode 0x0e = tick 6 $ absolute (asl . memory)
        decode 0x1e = tick 7 $ indexed X (asl . memory)
        -- LSR
        decode 0x4a = tick 2 $ lsr (register A)
        decode 0x46 = tick 5 $ zp (lsr . memory)
        decode 0x56 = tick 6 $ zpRel X (lsr . memory)
        decode 0x4e = tick 6 $ absolute (lsr . memory)
        decode 0x5e = tick 7 $ indexed X (lsr . memory)
        -- ROL
        decode 0x2a = tick 2 $ rol (register A)
        decode 0x26 = tick 5 $ zp (rol . memory)
        decode 0x36 = tick 6 $ zpRel X (rol . memory)
        decode 0x2e = tick 6 $ absolute (rol . memory)
        decode 0x3e = tick 7 $ indexed X (rol . memory)
        -- ROR
        decode 0x6a = tick 2 $ ror (register A)
        decode 0x66 = tick 5 $ zp (ror . memory)
        decode 0x76 = tick 6 $ zpRel X (ror . memory)
        decode 0x6e = tick 6 $ absolute (ror . memory)
        decode 0x7e = tick 7 $ indexed X (ror . memory)
        -- JMP
        decode 0x4c = tick 3 $ absolute jump
        decode 0x6c = tick 5 $ indirect jump
        -- JSR
        decode 0x20 = tick 6 $ absolute jsr
        -- RTS
        decode 0x60 = tick 6 $ rts
        -- branch instructions
        decode 0x90 = tick 2 $ branchNot Carry
        decode 0xb0 = tick 2 $ branchIf Carry
        decode 0xd0 = tick 2 $ branchNot Zero
        decode 0xf0 = tick 2 $ branchIf Zero
        decode 0x10 = tick 2 $ branchNot Negative
        decode 0x30 = tick 2 $ branchIf Negative
        decode 0x50 = tick 2 $ branchNot Overflow
        decode 0x70 = tick 2 $ branchIf Overflow
        -- flag instructions
        decode 0x18 = tick 2 $ setFlag Carry (bit False) done
        decode 0xd8 = tick 2 $ setFlag Decimal (bit False) done
        decode 0x58 = tick 2 $ setFlag InterruptDisable (bit False) done
        decode 0xb8 = tick 2 $ setFlag Overflow (bit False) done
        decode 0x38 = tick 2 $ setFlag Carry (bit True) done
        decode 0xf8 = tick 2 $ setFlag Decimal (bit True) done
        decode 0x78 = tick 2 $ setFlag InterruptDisable (bit True) done
        -- BRK
        decode 0x00 = tick 7 $ brk
        -- NOP
        decode 0xea = tick 2 $ done
        -- RTI
        decode 0x40 = tick 6 $ rti

        ldaMem = ldMem A
        ldxMem = ldMem X
        ldyMem = ldMem Y
        ldMem r addr = peek (memory addr) (ld r)
        ld r v =
          poke (register r) v $
          zeroNeg v $
          done
        sta = st A
        stx = st X
        sty = st Y
        st r addr =
          peek (register r) $ \x ->
          poke (memory addr) x $
          done
          
        transfer r1 r2 =
          peek (register r1) $ \x ->
          poke (register r2) x $
          zeroNeg x $
          done

        php break k=
          flag Negative $ \b7 ->
          flag Overflow $ \b6 ->
          flag Decimal $ \b3 ->
          flag InterruptDisable $ \b2 ->
          flag Zero $ \b1 ->
          flag Carry $ \b0 ->
          push (oneBit 0 b0 `or_`
                oneBit 1 b1 `or_`
                oneBit 2 b2 `or_`
                oneBit 3 b3 `or_`
                oneBit 4 break `or_`
                oneBit 5 (bit True) `or_`
                oneBit 6 b6 `or_`
                oneBit 7 b7) $
          k
        plp k =
          pop $ \x ->
          setFlag Negative (selectBit 7 x) $
          setFlag Overflow (selectBit 6 x) $
          setFlag Decimal (selectBit 3 x) $
          setFlag InterruptDisable (selectBit 2 x) $
          setFlag Zero (selectBit 1 x) $
          setFlag Carry (selectBit 0 x) $
          k

        accMem op addr = peek (memory addr) (acc op)
        acc op x =
          peek (register A) $ \y ->
          poke (register A) (x `op` y) $
          zeroNeg (x `op` y) $
          done

        bitTest addr =
          peek (register A) $ \x ->
          peek (memory addr) $ \y ->
          setFlag Zero (zero (x `and_` y)) $
          setFlag Negative (selectBit 7 y) $
          setFlag Overflow (selectBit 6 y) $
          done

        adcMem addr = peek (memory addr) adc
        adc x =
          flag Decimal $ \f ->
          cond f (adcBCD x) (adcNormal (register A) x)
          
        adcBCD x = adcNormal l (fromBCD x)
          where l = Location {
                  peek = \k -> peek (register A) (k . fromBCD),
                  poke = \v k ->
                   poke (register A) (toBCD v) $
                   setFlag Carry (geq v (byte 100)) $
                   k }
          
        adcNormal l x =
          flag Carry $ \c ->
          peek l $ \y ->
          let !z = x `add` y `add` oneBit 0 c
              !zc = bitOp or_ (x `carry` y) ((x `add` y) `carry` oneBit 0 c)
              x `equ` y = x `xor` y `xor` byte (-1) in
          zeroNeg z $
          setFlag Carry zc $
          setFlag Negative (selectBit 7 ((x `equ` y) `xor` z)) $
          -- important that this goes at the end so that poke can set the carry flag
          -- in BCD mode
          poke l z $
          done

        sbcMem addr = peek (memory addr) sbc
        sbc x = 
          flag Decimal $ \f ->
          cond f (sbcBCD x) (adcNormal (register A) (xor x (byte (-1))))

        sbcBCD x = adcNormal l (xor (fromBCD x) (byte (-1)))
          where l = Location {
                  peek = \k -> peek (register A) (k . fromBCD),
                  poke = \v k -> poke (register A) (toBCD v) k }

        cmpMem r addr = peek (memory addr) (cmp r)
        cmp r x =
          peek (register r) $ \y ->
          setFlag Carry (y `geq` x) $
          setFlag Zero (zero (x `xor` y)) $
          setFlag Negative (selectBit 7 (x `add` y)) $
          done

        inc = adjust (byte 1)
        dec = adjust (byte (-1))
        adjust x l =
          peek l $ \y ->
          poke l (x `add` y) $
          zeroNeg (x `add` y) $
          done

        asl l = rotate l leftRotate (bit False)
        lsr l = rotate l rightRotate (bit False)
        rol l = flag Carry $ rotate l leftRotate
        ror l = flag Carry $ rotate l rightRotate
        
        rotate l f c =
          peek l $ \x ->
          let !(x', c') = f x c in
          poke l x' $
          setFlag Carry c' $
          zeroNeg x' $
          done
        
        leftRotate x c = (x `shl` c, selectBit 7 x)
        rightRotate x c = (x `shr` c, selectBit 0 x)

        jump addr =
          storePC addr $
          done

        jsr addr =
          loadPC $ \pc ->
          push16 (pc `index` byte (-1)) $
          jump addr
        
        rts =
          pop16 $ \pc ->
          storePC (pc `index` byte 1) $
          done

        branchIf f =
          flag f $ \v ->
          relative $ \addr ->
          cond v (jump addr) done
          
        branchNot f =
          flag f $ \v ->
          relative $ \addr ->
          cond v done (jump addr)

        brk =
          loadPC $ \pc ->
          push16 (pc `index` byte (-1)) $
          php (bit True) $
          peek16 (address 0xfffe) $ \pc' ->
          jump pc'

        rti =
          plp $
          pop16 $ \pc ->
          jump (pc `index` byte 2)

        zeroNeg v k =
          setFlag Zero (zero v) $
          setFlag Negative (selectBit 7 v) $
          k

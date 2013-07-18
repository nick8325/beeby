-- An interpreter backend for the 6502 emulator.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, BangPatterns, TypeFamilies, Rank2Types, TypeSynonymInstances, MultiParamTypeClasses #-}
module Six502.Interpreter where

import Prelude hiding (abs)
import GHC.Prim
import Data.Primitive.ByteArray
import Six502.Machine
import Six502.Memory
import Control.Monad
import Data.Word
import Data.Int
import Data.Bits hiding (xor, bit)
import qualified Data.Bits
import Text.Printf

type RAM = MutableByteArray RealWorld

newRAM :: IO RAM
newRAM = do
  ram <- newByteArray 0x10000
  fill ram 0 0x10000 0xff
  return ram

instance IODevice RAM where
  range _ = (0, 0x10000)
  peekDevice = readByteArray
  pokeDevice = writeByteArray

instance AddressSpace (Step mem) RAM where
  {-# INLINE peekAddress #-}
  peekAddress mem = peekIO (readByteArray mem)
  {-# INLINE pokeAddress #-}
  pokeAddress mem = pokeIO (writeByteArray mem)

instance IOMachine (Step mem) where
  {-# INLINE peekIO #-}
  peekIO f addr = liftM (Byte . fromIntegral) (liftIO (f (fromAddr addr)))
  {-# INLINE pokeIO #-}
  pokeIO f addr (Byte v) = liftIO (f (fromAddr addr) (fromIntegral v))

newtype Step mem a = Step { run0 :: forall b. mem -> (a -> S -> IO b) -> S -> IO b }

run :: Step mem a -> mem -> S -> IO (a, S)
run x !mem !state = run0 x mem (\ res state' -> return (res, state')) state

instance Monad (Step mem) where
  return x = Step (\ !_ k !s -> k x s)
  x >>= f =
    Step (\ !m k !s -> run0 x m (\y -> run0 (f y) m k) s)

forever :: Step mem () -> Step mem ()
forever x = Step (\ !m _ ->
                   let k = run0 x m (const k)
                   in k)

mem :: Step mem mem
mem = Step (\ !m k s -> k m s)

gets :: (S -> a) -> Step mem a
gets f = Step (\ !m k s -> k (f s) s)

modify :: (S -> S) -> Step mem ()
modify f = Step (\ !_ k s -> k () (f s))

liftIO :: IO a -> Step mem a
liftIO x = Step (\ !_ k s -> x >>= \x' -> k x' s)

data S = S {
  rA, rX, rY, rStack :: {-# UNPACK #-} !Int,
  fCarry, fZero, fInterruptDisable, fDecimal, fOverflow, fNegative :: {-# UNPACK #-} !Int,
  pc :: {-# UNPACK #-} !Int,
  ticks :: {-# UNPACK #-} !Int
  }

instance Show S where
  show s =
    printf "A=%02x X=%02x Y=%02x SP=%02x PC=%04x %10d"
      (reg rA) (reg rX) (reg rY)
      (reg rStack) (pc s `mod` 0x10000) (ticks s) ++
    showFlag "CF" fCarry ++
    showFlag "ZF" fZero ++
    showFlag "IF" fInterruptDisable ++
    showFlag "DF" fDecimal ++
    showFlag "OF" fOverflow ++
    showFlag "NF" fNegative
      where reg r = r s `mod` 256
            showFlag name f | f s /= 0 = " " ++ name
                            | otherwise = ""

s0 :: S
s0 = S 0 0 0 0 1 1 1 1 1 1 0 0

fromAddr :: Addr (Step mem) -> Int
fromAddr (Addr x) = fromIntegral (fromIntegral x :: Word16)

fromByte :: Byte (Step mem) -> Int
fromByte (Byte x) = fromIntegral (fromIntegral x :: Word8)

fromSignedByte :: Byte (Step mem) -> Int
fromSignedByte (Byte x) = fromIntegral (fromIntegral x :: Int8)

instance MemorylessMachine (Step mem) where
  -- It simplifies the generated code considerably to let GHC just use Ints everywhere.
  -- We let addresses and bytes be arbitrary integers, i.e., out-of-bounds:
  -- "Byte x" really represents the byte "x `mod` 256".
  -- We use fromByte and fromAddr to truncate the integers when necessary.
  newtype Addr (Step mem) = Addr Int
  newtype Byte (Step mem) = Byte Int
  -- Representation: 0 is true, anything else is false
  newtype Bit (Step mem) = Bit Int

  {-# INLINE address #-}
  address = Addr
  {-# INLINE index #-}
  index (Addr x) b = Addr (x + fromByte b)
  {-# INLINE signedIndex #-}
  signedIndex (Addr x) b = Addr (x + fromSignedByte b)
  {-# INLINE page #-}
  page (Addr x) = Byte (x `shiftR` 8)
  {-# INLINE offset #-}
  offset (Addr x) = Byte x
  {-# INLINE paged #-}
  paged x y = Addr (fromByte x `shiftL` 8 + fromByte y)
  {-# INLINE byte #-}
  byte = Byte
  {-# INLINE bit #-}
  bit False = Bit (-1)
  bit True = Bit 0

  {-# INLINE shl #-}
  shl (Byte x) = Byte (x `shiftL` 1)
  {-# INLINE shr #-}
  shr x = Byte (fromByte x `shiftR` 1)
  {-# INLINE selectBit #-}
  selectBit n (Byte x) = Bit (complement x .&. Data.Bits.bit n)
  {-# INLINE oneBit #-}
  oneBit n (Bit 0) = Byte (Data.Bits.bit n)
  oneBit n (Bit _) = Byte 0
  {-# INLINE zero #-}
  zero x = Bit (fromByte x)
  {-# INLINE eq #-}
  eq x y = Bit (fromByte x `Data.Bits.xor` fromByte y)
  {-# INLINE geq #-}
  geq x y = Bit ((fromByte x - fromByte y) .&. minBound)

  {-# INLINE add #-}
  add (Byte x) (Byte y) = Byte (x + y)
  {-# INLINE carry #-}
  carry x y = Bit (((fromByte x + fromByte y) .&. 256) `Data.Bits.xor` 256)

  toBCD x =
    Byte (((fromByte x `div` 10) `shiftL` 4) + (fromByte x `mod` 10))
  fromBCD x =
    Byte (10*(fromByte x `shiftL` 4) + (fromByte x .&. 15))

  {-# INLINE and_ #-}
  and_ (Byte x) (Byte y) = Byte (x .&. y)
  {-# INLINE or_ #-}
  or_ (Byte x) (Byte y) = Byte (x .|. y)
  {-# INLINE xor #-}
  xor (Byte x) (Byte y) = Byte (x `Data.Bits.xor` y)
  {-# INLINE bitOr #-}
  bitOr (Bit 0) _ = Bit 0
  bitOr _ (Bit y) = Bit y

  {-# INLINE register #-}
  register A =
    Location { peek = gets (Byte . rA),
               poke = \(Byte x) -> modify (\s -> s { rA = x }) }

  register X =
    Location { peek = gets (Byte . rX),
               poke = \(Byte x) -> modify (\s -> s { rX = x }) }

  register Y =
    Location { peek = gets (Byte . rY),
               poke = \(Byte x) -> modify (\s -> s { rY = x }) }

  register Stack =
    Location { peek = gets (Byte . rStack),
               poke = \(Byte x) -> modify (\s -> s { rStack = x }) }

  {-# INLINE flag #-}
  flag Carry = gets (Bit . fCarry)
  flag Zero = gets (Bit . fZero)
  flag InterruptDisable = gets (Bit . fInterruptDisable)
  flag Decimal = gets (Bit . fDecimal)
  flag Overflow = gets (Bit . fOverflow)
  flag Negative = gets (Bit . fNegative)

  {-# INLINE setFlag #-}
  setFlag Carry (Bit x) = modify (\s -> s { fCarry = x })
  setFlag Zero (Bit x) = modify (\s -> s { fZero = x })
  setFlag InterruptDisable (Bit x) = modify (\s -> s { fInterruptDisable = x })
  setFlag Decimal (Bit x) = modify (\s -> s { fDecimal = x })
  setFlag Overflow (Bit x) = modify (\s -> s { fOverflow = x })
  setFlag Negative (Bit x) = modify (\s -> s { fNegative = x })

  {-# INLINE loadPC #-}
  loadPC = gets (Addr . pc)
  {-# INLINE storePC #-}
  storePC (Addr x) = modify (\s -> s { pc = x })

  {-# INLINE cond #-}
  cond (Bit x) p1 p2 = if x == 0 then p1 else p2
  {-# INLINE condRange #-}
  condRange addr (l, h) p1 p2 = if fromAddr addr >= l && fromAddr addr < h then p1 else p2
  {-# INLINE case_ #-}
  case_ x f = f (fromByte x)

  {-# INLINE tick #-}
  tick !n = modify (\s -> s { ticks = ticks s+n })

  {-# INLINE machineError #-}
  machineError = error

instance AddressSpace (Step mem) mem => Machine (Step mem) where
  {-# INLINE memory #-}
  memory addr =
    Location { peek = do { m <- mem; peekAddress m addr },
               poke = \v -> do { m <- mem; pokeAddress m addr v } }
  {-# INLINE fetch #-}
  fetch = do
    addr@(Addr pc) <- loadPC
    storePC (Addr (pc+1))
    m <- mem
    fetchAddress m addr
{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving, BangPatterns, TypeFamilies, Rank2Types #-}
-- A 6502 simulator.

module Six502.Simulator where

import Prelude hiding (abs)
import GHC.Types
import GHC.Prim
import Six502
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Data.Word
import Data.Int
import Data.Bits hiding (xor)
import qualified Data.Bits

type Memory s = STUArray s Int Int8
newtype Step s a = Step { run :: forall b. Memory s -> (a -> Sf (ST s b)) -> Sf (ST s b) }

instance Monad (Step s) where
  return x = Step (\ !_ k -> abs (\s -> apply (k x) s))
  x >>= f =
    Step (\ !m k -> run x m (\y -> run (f y) m k))

forever :: Step s () -> Step s ()
forever x = Step (\ !m _ ->
                   let k = run x m (const k)
                   in k)

mem :: Step s (Memory s)
mem = Step (\ !m k -> k m)

gets :: (S -> a) -> Step s a
gets f = Step (\ !m k -> abs (\s -> apply (k (f s)) s))

modify :: (S -> S) -> Step s ()
modify f = Step (\ !_ k -> abs (\s -> apply (k ()) (f s)))

liftST :: ST s a -> Step s a
liftST x = Step (\ !_ !k -> abs (\s -> x >>= \x' -> apply (k x') s))

data S = S {
  rA, rX, rY, rStack :: {-# UNPACK #-} !Int,
  fCarry, fZero, fInterruptDisable, fDecimal, fOverflow, fNegative :: !Bool,
  pc :: {-# UNPACK #-} !Int,
  ticks :: {-# UNPACK #-} !Int
  }
type Sf a = Int# -> Int# -> Int# -> Int# ->
            Bool -> Bool -> Bool -> Bool -> Bool -> Bool ->
            Int# -> Int# -> a
{-# INLINE apply #-}
apply :: Sf a -> S -> a
apply func (S (I# a) (I# b) (I# c) (I# d) e f g h i j (I# k) (I# l))
  = func a b c d e f g h i j k l
{-# INLINE abs #-}
abs :: (S -> a) -> Sf a
abs func a b c d e f g h i j k l
  = func (S (I# a) (I# b) (I# c) (I# d) e f g h i j (I# k) (I# l))

{-# INLINE fromAddr #-}
fromAddr :: Addr (Step s) -> Int
fromAddr (Addr x) = fromIntegral (fromIntegral x :: Word16)

{-# INLINE fromByte #-}
fromByte :: Byte (Step s) -> Int
fromByte (Byte x) = fromIntegral (fromIntegral x :: Word8)

{-# INLINE fromSignedByte #-}
fromSignedByte :: Byte (Step s) -> Int
fromSignedByte (Byte x) = fromIntegral (fromIntegral x :: Int8)

{-# INLINE peekMemory #-}
peekMemory :: Addr (Step s) -> Step s Int
peekMemory !addr = do
  mem <- mem
  liftM fromIntegral (liftST (unsafeRead mem (fromAddr addr)))

{-# INLINE pokeMemory #-}
pokeMemory :: Addr (Step s) -> Byte (Step s) -> Step s ()
pokeMemory !addr !(Byte x) = do
  mem <- mem
  liftST (unsafeWrite mem (fromAddr addr) (fromIntegral x))

instance Machine (Step s) where
  -- It simplifies the generated code considerably to let GHC just use Ints everywhere.
  -- We let addresses and bytes be arbitrary integers, i.e., out-of-bounds:
  -- "Byte x" really represents the byte "x `mod` 256".
  -- We use fromByte and fromAddr to truncate the integers when necessary.
  newtype Addr (Step s) = Addr Int
  newtype Byte (Step s) = Byte Int
  newtype Bit (Step s) = Bit Bool

  {-# INLINE address #-}
  address = Addr
  {-# INLINE index #-}
  index (Addr x) b = Addr (x + fromSignedByte b)
  {-# INLINE page #-}
  page (Addr x) = Byte (x `shiftR` 8)
  {-# INLINE offset #-}
  offset (Addr x) = Byte x
  {-# INLINE paged #-}
  paged x y = Addr (fromByte x `shiftL` 8 + fromByte y)
  {-# INLINE byte #-}
  byte = Byte
  {-# INLINE bit #-}
  bit = Bit

  {-# INLINE shl #-}
  shl (Byte x) = Byte (x `shiftL` 1)
  {-# INLINE shr #-}
  shr x = Byte (fromByte x `shiftR` 1)
  {-# INLINE selectBit #-}
  selectBit n (Byte x) = Bit (x `testBit` n)
  {-# INLINE oneBit #-}
  oneBit n (Bit False) = Byte 0
  oneBit n (Bit True) = Byte (Data.Bits.bit n)
  {-# INLINE zero #-}
  zero (Byte x) = Bit (x == 0)
  {-# INLINE eq #-}
  eq (Byte x) (Byte y) = Bit (x == y)
  {-# INLINE geq #-}
  geq (Byte x) (Byte y) = Bit (x >= y)
  
  {-# INLINE add #-}
  add (Byte x) (Byte y) = Byte (x + y)
  {-# INLINE carry #-}
  carry x y = Bit (fromByte x + fromByte y >= 256)

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
  bitOr (Bit x) (Bit y) = Bit (x || y)

  {-# INLINE memory #-}
  memory x =
    Location { peek = liftM Byte (peekMemory x),
               poke = pokeMemory x }

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
  cond (Bit x) p1 p2 = if x then p1 else p2
  {-# INLINE fetch #-}
  fetch = do
    addr@(Addr pc) <- loadPC
    x <- peekMemory addr
    storePC (Addr (pc+1))
    return (fromIntegral x)

  {-# INLINE tick #-}
  tick !n = modify (\s -> s { ticks = ticks s+n })

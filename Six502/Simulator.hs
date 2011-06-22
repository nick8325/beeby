{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TypeFamilies #-}
-- A 6502 simulator.

module Six502.Simulator where

import Six502
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Data.Word
import Data.Int
import Data.Bits hiding (xor)
import qualified Data.Bits

type Memory s = STUArray s Int Int8
newtype Step s a = Step (ReaderT (Memory s) (StateT S (ST s)) a)
  deriving Monad

liftReader :: ReaderT (Memory s) (StateT S (ST s)) a -> Step s a
liftReader = Step

liftState :: StateT S (ST s) a -> Step s a
liftState = Step . lift

liftST :: ST s a -> Step s a
liftST = Step . lift . lift

run :: Step s a -> Memory s -> S -> ST s (a, S)
run (Step x) !mem !state = runStateT (runReaderT x mem) state

data S = S {
  rA, rX, rY, rStack :: {-# UNPACK #-} !Int,
  fCarry, fZero, fInterruptDisable, fDecimal, fOverflow, fNegative :: !Bool,
  pc :: {-# UNPACK #-} !Int,
  ticks :: {-# UNPACK #-} !Int
  }

{-# INLINE fromBit #-}
fromBit :: Bit (Step s) -> Int
fromBit (Bit True) = 1
fromBit (Bit False) = 0

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
  mem <- liftReader ask
  liftM fromIntegral (liftST (unsafeRead mem (fromAddr addr)))

{-# INLINE pokeMemory #-}
pokeMemory :: Addr (Step s) -> Byte (Step s) -> Step s ()
pokeMemory !addr !(Byte x) = do
  mem <- liftReader ask
  liftST (unsafeWrite mem (fromAddr addr) (fromIntegral x))

instance Machine (Step s) where
  -- It simplifies the generated code considerably to let GHC just use Ints everywhere.
  -- We let addresses and bytes be arbitrary integers, i.e., out-of-bounds:
  -- "Byte x" really represents the byte "x `mod` 256".
  -- We use fromByte and fromAddr to truncate the integers when necessary.
  newtype Addr (Step s) = Addr Int
  newtype Byte (Step s) = Byte Int
  newtype Bit (Step s) = Bit Bool

  address = Addr
  index (Addr x) b = Addr (x + fromSignedByte b)
  page (Addr x) = Byte (x `shiftR` 8)
  offset (Addr x) = Byte x
  {-# INLINE paged #-}
  paged x y = Addr (fromByte x `shiftL` 8 + fromByte y)
  byte = Byte
  bit = Bit

  shl (Byte x) = Byte (x `shiftL` 1)
  shr x = Byte (fromByte x `shiftR` 1)
  selectBit n (Byte x) = Bit (testBit x n)
  oneBit n b = Byte (fromBit b `shiftL` n)
  zero (Byte x) = Bit (x == 0)
  eq (Byte x) (Byte y) = Bit (x == y)
  geq (Byte x) (Byte y) = Bit (x >= y)
  
  add (Byte x) (Byte y) = Byte (x + y)
  {-# INLINE carry #-}
  carry x y = Bit (fromByte x + fromByte y >= 256)
  {-# INLINE toBCD #-}
  toBCD x =
    Byte (((fromByte x `div` 10) `shiftL` 4) + (fromByte x `mod` 10))
  {-# INLINE fromBCD #-}
  fromBCD x =
    Byte (10*(fromByte x `shiftL` 4) + (fromByte x .&. 15))
  and_ (Byte x) (Byte y) = Byte (x .&. y)
  or_ (Byte x) (Byte y) = Byte (x .|. y)
  xor (Byte x) (Byte y) = Byte (x `Data.Bits.xor` y)
  bitOr (Bit x) (Bit y) = Bit (x || y)

  {-# INLINE memory #-}
  memory x =
    Location { peek = liftM Byte (peekMemory x),
               poke = pokeMemory x }

  register A = 
    Location { peek = liftState (gets (Byte . rA)),
               poke = \(Byte x) -> liftState (modify (\s -> s { rA = x })) }

  register X = 
    Location { peek = liftState (gets (Byte . rX)),
               poke = \(Byte x) -> liftState (modify (\s -> s { rX = x })) }

  register Y = 
    Location { peek = liftState (gets (Byte . rY)),
               poke = \(Byte x) -> liftState (modify (\s -> s { rY = x })) }

  register Stack = 
    Location { peek = liftState (gets (Byte . rStack)),
               poke = \(Byte x) -> liftState (modify (\s -> s { rStack = x })) }

  flag Carry = liftState (gets (Bit . fCarry))
  flag Zero = liftState (gets (Bit . fZero))
  flag InterruptDisable = liftState (gets (Bit . fInterruptDisable))
  flag Decimal = liftState (gets (Bit . fDecimal))
  flag Overflow = liftState (gets (Bit . fOverflow))
  flag Negative = liftState (gets (Bit . fNegative))
  
  setFlag Carry (Bit x) = liftState (modify (\s -> s { fCarry = x }))
  setFlag Zero (Bit x) = liftState (modify (\s -> s { fZero = x }))
  setFlag InterruptDisable (Bit x) = liftState (modify (\s -> s { fInterruptDisable = x }))
  setFlag Decimal (Bit x) = liftState (modify (\s -> s { fDecimal = x }))
  setFlag Overflow (Bit x) = liftState (modify (\s -> s { fOverflow = x }))
  setFlag Negative (Bit x) = liftState (modify (\s -> s { fNegative = x }))

  loadPC = liftState (gets (Addr . pc))
  storePC (Addr x) = liftState (modify (\s -> s { pc = x }))

  cond (Bit x) p1 p2 = if x then p1 else p2
  {-# INLINE fetch #-}
  fetch = do
    addr@(Addr pc) <- loadPC
    x <- peekMemory addr
    storePC (Addr (pc+1))
    return (fromIntegral x)

  tick !n = do
    x <- liftState (gets ticks)
    liftState (modify (\s -> s { ticks = x+n }))

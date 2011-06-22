{-# LANGUAGE BangPatterns, TypeFamilies #-}
-- A 6502 simulator.

module Six502.Simulator where

import Six502
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Data.Word
import Data.Int
import Data.Bits hiding (xor)
import qualified Data.Bits

type Memory s = STUArray s Int Int8
newtype Step s = Step (Memory s -> State -> ST s State)

run :: Step s -> Memory s -> State -> ST s State
run (Step f) !mem !state = f mem state

data State = State {
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
peekMemory :: Addr (Step s) -> (Int -> Step s) -> Step s
peekMemory !addr k = Step f
  where {-# INLINE f #-}
        f !mem !state =
          do
            x <- unsafeRead mem (fromAddr addr)
            run (k (fromIntegral x)) mem state

{-# INLINE pokeMemory #-}
pokeMemory :: Addr (Step s) -> Byte (Step s) -> Step s -> Step s
pokeMemory !addr !(Byte x) k = Step f
  where {-# INLINE f #-}
        f !mem !state =
          do
            unsafeWrite mem (fromAddr addr) (fromIntegral x)
            run k mem state

{-# INLINE peekState #-}
peekState :: (State -> a) -> (a -> Step s) -> Step s
peekState get k = Step f
  where {-# INLINE f #-}
        f !mem !state = run (k (get state)) mem state
        
{-# INLINE pokeState #-}
pokeState :: (State -> State) -> Step s -> Step s
pokeState put k = Step f
  where {-# INLINE f #-}
        f !mem !state = run k mem (put state)

instance Program (Step s) where
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
    Location { peek = \k -> peekMemory x (k . Byte),
               poke = pokeMemory x }

  register A = 
    Location { peek = peekState (Byte . rA),
               poke = \(Byte x) -> pokeState (\s -> s { rA = x }) }

  register X = 
    Location { peek = peekState (Byte . rX),
               poke = \(Byte x) -> pokeState (\s -> s { rX = x }) }

  register Y  = 
    Location { peek = peekState (Byte . rY),
               poke = \(Byte x) -> pokeState (\s -> s { rY = x }) }

  register Stack = 
    Location { peek = peekState (Byte . rStack),
               poke = \(Byte x) -> pokeState (\s -> s { rStack = x }) }

  flag Carry = peekState (Bit . fCarry)
  flag Zero = peekState (Bit . fZero)
  flag InterruptDisable = peekState (Bit . fInterruptDisable)
  flag Decimal = peekState (Bit . fDecimal)
  flag Overflow = peekState (Bit . fOverflow)
  flag Negative = peekState (Bit . fNegative)
  
  setFlag Carry (Bit x) = pokeState (\s -> s { fCarry = x })
  setFlag Zero (Bit x) = pokeState (\s -> s { fZero = x })
  setFlag InterruptDisable (Bit x) = pokeState (\s -> s { fInterruptDisable = x })
  setFlag Decimal (Bit x) = pokeState (\s -> s { fDecimal = x })
  setFlag Overflow (Bit x) = pokeState (\s -> s { fOverflow = x })
  setFlag Negative (Bit x) = pokeState (\s -> s { fNegative = x })

  loadPC = peekState (Addr . pc)
  storePC (Addr x) = pokeState (\s -> s { pc = x })

  cond (Bit x) p1 p2 = if x then p1 else p2
  {-# INLINE fetch #-}
  fetch k =
    loadPC $ \addr@(Addr pc) ->
    peekMemory addr $ \x ->
    storePC (Addr (pc+1)) $
    k (fromIntegral x)

  tick !n k =
    peekState ticks $ \x ->
    pokeState (\s -> s { ticks = x+n }) $
    k

  done = Step (const return)

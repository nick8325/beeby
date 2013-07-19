-- The Machine class, for executing the 6502 emulator in Six502.CPU.
-- Instances of this class are known as backends.

{-# LANGUAGE TypeFamilies #-}
module Six502.Machine where

import Data.Int

-- A location: something you can peek or poke.
data Location m =
  Location { peek :: m (Byte m),
             poke :: Byte m -> m () }

-- The 6502's registers and flags.
data Flag = Carry | Zero | InterruptDisable | Decimal | Overflow | Negative
data Register = A | X | Y | Stack

-- The machine monad: a set of combinators for describing the processor.
-- Can be instantiated for an interpreter, a JIT compiler, etc.
class Monad m => MemorylessMachine m where
  data Addr m
  data Byte m
  data Bit m

  address :: Int -> Addr m
  index :: Addr m -> Byte m -> Addr m
  signedIndex :: Addr m -> Byte m -> Addr m
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

  cond :: Bit m -> m a -> m a -> m a
  condRange :: Addr m -> (Int, Int) -> m a -> m a -> m a
  case_ :: Byte m -> (Int -> m a) -> m a
  register :: Register -> Location m
  flag :: Flag -> m (Bit m)
  setFlag :: Flag -> Bit m -> m ()
  loadPC :: m (Addr m)
  storePC :: Addr m -> m ()

  tick :: Int64 -> m ()
  currentTicks :: m Int64
  machineError :: String -> m a

class MemorylessMachine m => Machine m where
  memory :: Addr m -> Location m
  fetch :: m (Byte m)
{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving, BangPatterns, TypeFamilies, Rank2Types #-}
-- A 6502 simulator.

module Six502.Simulator where

import Prelude hiding (abs)
import GHC.Types
import GHC.Prim
import Six502
import Control.Monad
import Data.Array.Base
import Data.Array.IO
import Data.Word
import Data.Int
import Data.Bits hiding (xor)
import qualified Data.Bits
import Numeric

type Memory = IOUArray Int Word8
newtype Step a = Step { run0 :: forall b. Memory -> (a -> Sf (IO b)) -> Sf (IO b) }

run :: Step a -> Memory -> S -> IO (a, S)
run x !mem !state = apply (run0 x mem (\ !res -> abs (\ !state' -> return (res, state')))) state

instance Monad Step where
  return x = Step (\ !_ k -> abs (\s -> apply (k x) s))
  x >>= f =
    Step (\ !m k -> run0 x m (\y -> run0 (f y) m k))

forever :: Step () -> Step ()
forever x = Step (\ !m _ ->
                   let k = run0 x m (const k)
                   in k)

mem :: Step Memory
mem = Step (\ !m k -> k m)

gets :: (S -> a) -> Step a
gets f = Step (\ !m k -> abs (\s -> apply (k (f s)) s))

modify :: (S -> S) -> Step ()
modify f = Step (\ !_ k -> abs (\s -> apply (k ()) (f s)))

liftIO :: IO a -> Step a
liftIO x = Step (\ !_ !k -> abs (\s -> x >>= \x' -> apply (k x') s))

data S = S {
  rA, rX, rY, rStack :: {-# UNPACK #-} !Int,
  fCarry, fZero, fInterruptDisable, fDecimal, fOverflow, fNegative :: !Bool,
  pc :: {-# UNPACK #-} !Int,
  ticks :: {-# UNPACK #-} !Int
  }
         
instance Show S where
  show s =
    show (ticks s) ++ " " ++
    showReg "A" rA ++
    showReg "X" rX ++
    showReg "Y" rY ++
    showReg "SP" rStack ++
    showFlag "CF" fCarry ++
    showFlag "ZF" fZero ++
    showFlag "IF" fInterruptDisable ++
    showFlag "DF" fDecimal ++
    showFlag "OF" fOverflow ++
    showFlag "NF" fNegative ++
    "PC=" ++ showHex (pc s `mod` 65536) ""
      where showReg name r = name ++ "=" ++ showHex (r s `mod` 256) " "
            showFlag name f | f s = name ++ " "
                            | otherwise = ""

s0 :: S
s0 = S 0 0 0 0 False False False False False False 0 0

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
fromAddr :: Addr Step -> Int
fromAddr (Addr x) = fromIntegral (fromIntegral x :: Word16)

{-# INLINE fromByte #-}
fromByte :: Byte Step -> Int
fromByte (Byte x) = fromIntegral (fromIntegral x :: Word8)

{-# INLINE fromSignedByte #-}
fromSignedByte :: Byte Step -> Int
fromSignedByte (Byte x) = fromIntegral (fromIntegral x :: Int8)

{-# INLINE peekMemory #-}
peekMemory :: Addr Step -> Step Int
peekMemory !addr = do
  mem <- mem
  res <- liftM fromIntegral (liftIO (unsafeRead mem (fromAddr addr)))
  -- liftIO $ putStrLn $ " reading " ++ showHex res "" ++ " from address " ++ showHex (fromAddr addr) ""
  return res

{-# INLINE pokeMemory #-}
pokeMemory :: Addr Step -> Byte Step -> Step ()
pokeMemory !addr !(Byte x) = do
  mem <- mem
  liftIO (unsafeWrite mem (fromAddr addr) (fromIntegral x))
  when ((fromAddr addr >= 0xfe00 && fromAddr addr < 0xff00) || False) $
    liftIO $
    putStrLn $ "writing " ++ showHex (fromByte (Byte x)) "" ++
               " to address " ++ showHex (fromAddr addr) ""

instance Machine Step where
  -- It simplifies the generated code considerably to let GHC just use Ints everywhere.
  -- We let addresses and bytes be arbitrary integers, i.e., out-of-bounds:
  -- "Byte x" really represents the byte "x `mod` 256".
  -- We use fromByte and fromAddr to truncate the integers when necessary.
  newtype Addr Step = Addr Int
  newtype Byte Step = Byte Int
  newtype Bit Step = Bit Bool

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
  zero x = Bit (fromByte x == 0)
  {-# INLINE eq #-}
  eq x y = Bit (fromByte x == fromByte y)
  {-# INLINE geq #-}
  geq x y = Bit (fromByte x >= fromByte y)
  
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

module BBC.Machine where

import Six502.Interpreter
import Six502.Memory
import qualified Data.ByteString as BS
import Numeric
import Data.IORef
import Data.Word
import Codec.PPM
import Data.Bits
import Control.Monad

type Memory = Overlay Sheila RAM

data Sheila = Sheila {
  ram :: RAM,
  basicROM :: BS.ByteString,
  pagedROM :: IORef Word8,
  videoRegister :: IORef Word8
  }

newSheila :: RAM -> IO Sheila
newSheila ram = do
  basicROM <- BS.readFile "BASIC2.ROM"
  pagedROM <- newIORef 0
  videoRegister <- newIORef 0
  return (Sheila ram basicROM pagedROM videoRegister)

pokePagedROM :: Sheila -> Word8 -> IO ()
pokePagedROM sheila n = do
  writeIORef (pagedROM sheila) n
  putStrLn $ "Load paged ROM " ++ show n
  case n of
    15 -> blit (ram sheila) 0x8000 (basicROM sheila)
    _ -> fill (ram sheila) 0x8000 0xc000 0xff

peekVideoRegister :: Word8 -> IO Word8
peekVideoRegister addr = do
  putStrLn $ "Reading video register " ++ show addr
  case addr of
    _ -> return 0

pokeVideoRegister :: Word8 -> Word8 -> IO ()
pokeVideoRegister value addr = do
  putStrLn $ "Video register " ++ show addr ++ " <- " ++ showHex value ""
  case addr of
    _ -> return ()

dumpScreen :: RAM -> IO ()
dumpScreen ram = do
  writePPM "bitmap" (320, 256) =<< pixels
  forM_ [0..24] $ \row -> do
    forM_ [0..39] $ \col -> do
      c <- peekDevice ram (0x7c00 + row*40 + col)
      putStr [toEnum (fromEnum c)]
    putStrLn ""
  where
    pixels = fmap (map toPixel) . sequence $
             [ pixel row col | row <- [0..255], col <- [0..319] ]
    toPixel True = (255,255,255)
    toPixel False = (0,0,0)
    pixel row col = do
      let colMaj = col `div` 8
          colMin = col `mod` 8
          rowMaj = row `div` 8
          rowMin = row `mod` 8
          idx = rowMaj * 320 + colMaj * 8 + rowMin
      val <- peekDevice ram (0x5800 + idx)
      return (testBit val (7 - colMin))

{-# NOINLINE peekSheila #-}
peekSheila :: Sheila -> Int -> IO Word8
peekSheila sheila addr =
  case addr of
    _ | addr >= 0xfe40 && addr < 0xfe50 -> return 0
    _ | addr >= 0xfe60 && addr < 0xfe70 -> return 0
    0xfe01 -> readIORef (videoRegister sheila) >>= peekVideoRegister
    0xfe30 -> readIORef (pagedROM sheila)
    _ -> do
      putStrLn (showHex addr "")
      return 0xff

{-# NOINLINE pokeSheila #-}
pokeSheila :: Sheila -> Int -> Word8 -> IO ()
pokeSheila sheila addr value =
  case addr of
    _ | addr >= 0xfe40 && addr < 0xfe50 -> return ()
    _ | addr >= 0xfe60 && addr < 0xfe70 -> return ()
    0xfe00 -> writeIORef (videoRegister sheila) value
    0xfe01 -> readIORef (videoRegister sheila) >>= pokeVideoRegister value
    0xfe30 -> pokePagedROM sheila value
    _ -> do
      putStrLn (showHex addr "" ++ " <- " ++ showHex value "")
      return ()

instance IODevice Sheila where
  range _ = (0xfe00, 0xff00)
  peekDevice sheila addr = peekSheila sheila addr
  pokeDevice sheila addr value = pokeSheila sheila addr value

newMachine :: IO Memory
newMachine = do
  ram <- newRAM
  sheila <- newSheila ram
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  return (Overlay sheila ram)
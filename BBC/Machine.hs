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
import Text.Printf

type Memory = Overlay Sheila RAM

data Sheila = Sheila {
  ram :: RAM,
  dispatch :: Int -> Register Word8
  }

newSheila :: RAM -> IO Sheila
newSheila ram = do
  basicROM <- BS.readFile "BASIC2.ROM"

  pagedROM <- newPagedROM ram $ \n ->
    case n of
      15 -> basicROM
      _ -> BS.replicate 0x4000 0xff

  (videoAddr, videoData) <- switch 0 $ \addr ->
    unknown "video register" addr

  let dispatch 0xfe30 = pagedROM
      dispatch 0xfe00 = videoAddr
      dispatch 0xfe01 = videoData
      dispatch addr = unknown "address" addr

  return (Sheila ram dispatch)

unknown xs addr =
  writeOnlyRegister 0 $ \val ->
    printf "Write of %02x to unknown %s %04x\n" val xs addr

data Register a = Register {
  readRegister :: IO a,
  writeRegister :: a -> IO ()
  }

register :: a -> IO (Register a)
register x = do
  r <- newIORef x
  return Register {
    readRegister = readIORef r,
    writeRegister = writeIORef r
    }

switch :: a -> (a -> Register b) -> IO (Register a, Register b)
switch def select = do
  reg <- register def
  let read = readRegister reg >>= readRegister . select
      write val = readRegister reg >>= flip writeRegister val . select
  return (reg, Register read write)

afterWrite :: Register a -> (a -> IO ()) -> Register a
afterWrite reg act = reg { writeRegister = \val -> writeRegister reg val >> act val }

constRegister :: a -> Register a
constRegister def = Register { readRegister = return def, writeRegister = const (return ()) }

activeRegister :: a -> (a -> IO ()) -> IO (Register a)
activeRegister def act = do
  act def
  reg <- register def
  return (reg `afterWrite` act)

writeOnlyRegister :: a -> (a -> IO ()) -> Register a
writeOnlyRegister def act =
  constRegister def `afterWrite` act

newPagedROM :: RAM -> (Word8 -> BS.ByteString) -> IO (Register Word8)
newPagedROM ram select =
  activeRegister 0 $ \bank -> do
    putStrLn $ "Load paged ROM " ++ show bank
    blit ram 0x8000 (select bank)

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
peekSheila sheila addr = readRegister (dispatch sheila addr)

{-# NOINLINE pokeSheila #-}
pokeSheila :: Sheila -> Int -> Word8 -> IO ()
pokeSheila sheila addr value =
  writeRegister (dispatch sheila addr) value

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
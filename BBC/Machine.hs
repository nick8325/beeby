module BBC.Machine where

import Six502.CPU
import Six502.Interpreter
import Six502.Memory
import Six502.System
import qualified Data.ByteString as BS
import Data.Word
import BBC.Register
import BBC.PagedROM
import BBC.Video
import Driver.Video

data Machine a = Machine {
  ram :: RAM,
  system :: System (Overlay Sheila RAM) a,
  pagedROM :: Register Word8,
  videoChip :: VideoChip,
  sheila :: Sheila
  }

newtype Sheila = Sheila {
  dispatch :: Int -> Register Word8
  }

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

newMachine :: VideoDriver -> IO (Machine a)
newMachine videoDriver = do
  system <- newSystem
  ram <- newRAM
  pagedROM <- newSinglePagedROM ram =<< BS.readFile "BASIC2.ROM"
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  videoChip <- newVideoChip videoDriver system ram

  -- Construct SHEILA
  (videoAddr, videoData) <- switch 0 (videoIO videoChip)
  let dispatch 0xfe30 = pagedROM
      dispatch 0xfe00 = videoAddr
      dispatch 0xfe01 = videoData
      dispatch addr = unknownRegister "address" addr

  return (Machine ram system pagedROM videoChip (Sheila dispatch))

runMachine :: Machine a -> IO a
runMachine machine =
  fmap fst
    (run (reset >> execute (system machine) cpu)
      (Overlay (sheila machine) (ram machine)) s0)

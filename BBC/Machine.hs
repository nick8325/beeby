module BBC.Machine where

import Six502.Interpreter
import Six502.Memory
import qualified Data.ByteString as BS
import Data.Word
import BBC.Register
import BBC.PagedROM

data Machine = Machine {
  ram :: RAM,
  pagedROM :: Register Word8,
  videoIO :: Word8 -> Register Word8
  }

newMachine :: IO Machine
newMachine = do
  ram <- newRAM
  pagedROM <- newSinglePagedROM ram =<< BS.readFile "BASIC2.ROM"
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  let videoIO = unknownRegister "video address"
  return (Machine ram pagedROM videoIO)
module BBC.Machine where

import Six502.Interpreter
import Six502.Memory
import qualified Data.ByteString as BS
import Data.Word
import BBC.Register
import BBC.PagedROM
import BBC.Video
import Driver.Video

data Machine = Machine {
  ram :: RAM,
  pagedROM :: Register Word8,
  videoChip :: VideoChip
  }

newMachine :: VideoDriver -> IO Machine
newMachine videoDriver = do
  ram <- newRAM
  pagedROM <- newSinglePagedROM ram =<< BS.readFile "BASIC2.ROM"
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  videoChip <- newVideoChip videoDriver
  return (Machine ram pagedROM videoChip)
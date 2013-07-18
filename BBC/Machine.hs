module BBC.Machine where

import Six502.Interpreter
import Six502.Memory
import qualified Data.ByteString as BS
import BBC.Sheila

type Memory = Overlay Sheila RAM

newMachine :: IO Memory
newMachine = do
  ram <- newRAM
  blit ram 0x8000 =<< BS.readFile "BASIC2.ROM"
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  return (Overlay Sheila ram)
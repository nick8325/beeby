module BBC.Machine where

import Six502.Interpreter
import Six502.Memory
import qualified Data.ByteString as BS

type Memory = Overlay Sheila RAM

data Sheila = Sheila

{-# NOINLINE peekSheila #-}
peekSheila Sheila addr =
  case addr of
    _ -> return 0xff

{-# NOINLINE pokeSheila #-}
pokeSheila Sheila addr value =
  return ()

instance IODevice Sheila where
  range _ = (0xfe00, 0xff00)
  peekDevice sheila addr = peekSheila sheila addr
  pokeDevice sheila addr value = pokeSheila sheila addr value

newMachine :: IO Memory
newMachine = do
  ram <- newRAM
  blit ram 0x8000 =<< BS.readFile "BASIC2.ROM"
  blit ram 0xc000 =<< BS.readFile "OS12.ROM"
  return (Overlay Sheila ram)
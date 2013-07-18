-- The "SHEILA" I/O page.
module BBC.Sheila where

import BBC.Machine
import BBC.Register
import Six502.Memory
import Data.Word

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

newSheila :: Machine -> IO Sheila
newSheila machine = do
  (videoAddr, videoData) <- switch 0 (videoIO machine)

  let dispatch 0xfe30 = pagedROM machine
      dispatch 0xfe00 = videoAddr
      dispatch 0xfe01 = videoData
      dispatch addr = unknownRegister "address" addr

  return (Sheila dispatch)

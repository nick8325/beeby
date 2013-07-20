-- The video chip.
module BBC.Video where

import BBC.Register
import BBC.CPU
import Six502.Interpreter
import Driver.Video
import Data.Word
import Data.Array
import Data.Array.IO
import Data.Bits
import Control.Monad
import qualified Data.Traversable as T

data VideoChip = VideoChip {
  videoIO :: Word8 -> Register Word8,
  -- Connected to system VIA.
  videoMemory :: Register Word16,
  -- Mapped into SHEILA at fe20-fe21.
  videoControl :: Register Word8,
  paletteControl :: Register Word8
  }

palette :: [Colour]
palette = [
  Colour 0 0 0,         -- Black
  Colour 0xff 0 0,      -- Red
  Colour 0 0xff 0,      -- Green
  Colour 0xff 0xff 0,   -- Yellow
  Colour 0 0 0xff,      -- Blue
  Colour 0xff 0 0xff,   -- Magenta
  Colour 0 0xff 0xff,   -- Cyan
  Colour 0xff 0xff 0xff -- White
  ]

-- Transform a BBC "physical colour" into a palette index
toPalette :: Bool -> Word8 -> Word8
toPalette flashOn n
  | n < 8 = n
  | flashOn = 15 - n
  | otherwise = n - 8

drawFrame :: VideoDriver -> RAM -> Word16 -> Word8 ->
             Array Word8 Word8 -> Array Word8 Word8 -> IO ()
drawFrame _ _ _ _ _ _ = return ()

newVideoChip :: VideoDriver -> CPU mem a -> RAM -> IO VideoChip
newVideoChip videoDriver cpu ram = do
  videoMemory <- register 0
  videoControl <- register 0

  palette <- newArray (0, 15) 0 :: IO (IOUArray Word8 Word8)
  let paletteControl = writeOnlyRegister 0 $ \val ->
        let logical = val .&. 0xf
            physical = val `shiftR` 4
        in writeArray palette logical physical

  registers <-
    fmap (listArray (0, 17))
      (replicateM 18 (register 0))
  let videoIO n | inRange (bounds registers) n = registers ! n
                | otherwise = unknownRegister "video register" n

  every cpu 40000 . liftIO $ do
    videoMemoryVal <- readRegister videoMemory
    videoControlVal <- readRegister videoControl
    paletteArr <- freeze palette
    registerValues <- T.mapM readRegister registers
    drawFrame videoDriver ram videoMemoryVal videoControlVal
      paletteArr registerValues
    return Nothing

  return (VideoChip videoIO videoMemory videoControl paletteControl)

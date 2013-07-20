-- The video chip.
module BBC.Video where

import BBC.Register
import Driver.Video
import Data.Word
import Data.Array.IO
import Data.Bits

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

newVideoChip :: VideoDriver -> IO VideoChip
newVideoChip videoDriver = do
  let videoIO = unknownRegister "video address"
  videoMemory <- register 0
  videoAddress <- register 0
  videoControl <- register 0

  palette <- newArray (0, 15) 0 :: IO (IOUArray Word8 Word8)
  let paletteControl = writeOnlyRegister 0 $ \val ->
        let logical = val .&. 0xf
            physical = val `shiftR` 4
        in writeArray palette logical physical

  return (VideoChip videoIO videoMemory videoControl paletteControl)
-- The video chip.
module BBC.Video where

import BBC.Register
import Driver.Video
import Data.Word

data VideoChip = VideoChip {
  videoIO :: Word8 -> Register Word8
  }

newVideoChip :: VideoDriver -> IO VideoChip
newVideoChip videoDriver = do
  let videoIO = unknownRegister "video address"
  return (VideoChip videoIO)
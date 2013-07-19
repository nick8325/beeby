-- Video output.
module Driver.Video where

import Data.Array.Unboxed
import Data.Word

type VideoDriver = ()
data Image = Image {
  width :: Int,
  height :: Int,
  red :: UArray (Int, Int) Word8,
  green :: UArray (Int, Int) Word8,
  blue :: UArray (Int, Int) Word8
  }

newVideoDriver :: IO VideoDriver
newVideoDriver = return ()

draw :: VideoDriver -> Image -> IO ()
draw _ _ = return ()

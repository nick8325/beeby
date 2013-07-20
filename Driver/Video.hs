-- Video output.
module Driver.Video where

import Data.Array.Unboxed
import Data.Array.Base
import Data.Word
import Graphics.UI.SDL
import Control.Concurrent
import Control.Concurrent.Chan
import Foreign
import Control.Monad

type VideoDriver = Chan Image
data Image = Image {
  width :: Int,
  height :: Int,
  palette :: [Colour],
  -- left to right, top to bottom
  pixels :: UArray Int Word8
  }
data Colour = Colour {
  red, green, blue :: Word8
  }

videoServer :: (Image -> IO Surface) -> Chan Image -> IO ()
videoServer getSurface chan = do
  image <- readChan chan
  surface <- getSurface image
  drawImage surface image
  let getSurface' image'
        | (width image, height image) /= (width image', height image') =
          newSurface image'
        | otherwise = return surface
  videoServer getSurface' chan

newSurface :: Image -> IO Surface
newSurface Image { width = width, height = height } =
  setVideoMode width height 8 []

drawImage :: Surface -> Image -> IO ()
drawImage surface image = do
  lockSurface surface
  let toColor (Colour r g b) = Color r g b
  setColors surface (map toColor (palette image)) 0
  buf <- fmap castPtr (surfaceGetPixels surface)
  forM_ [0..height image*width image-1] $ \x ->
      pokeElemOff buf x (pixels image `unsafeAt` x)
  unlockSurface surface
  Graphics.UI.SDL.flip surface
  return ()

newVideoDriver :: IO VideoDriver
newVideoDriver = do
  chan <- newChan
  forkIO (videoServer newSurface chan)
  return chan

draw :: VideoDriver -> Image -> IO ()
draw = writeChan

-- testing
pal = [Colour (i*5) (i*3) (i*11) | i <- [0..255] ]

img = Image 640 480 pal arr
arr = array (0, 640*480-1) [(i, fromIntegral i) | i <- [0..640*480-1]]

img' = Image 640 480 pal arr'
arr' = array (0, 640*480-1) [(i, fromIntegral (-i)) | i <- [0..640*480-1]]

go n vd = replicateM_ n (draw vd img >> draw vd img')
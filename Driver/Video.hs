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
  -- pixel format: pixel (i,j) is at i+j*width,
  -- 0xRRGGBB
  pixels :: UArray Int Word32
  }
type Surf = (Surface, Surface)

videoServer :: (Image -> IO Surf) -> Chan Image -> IO ()
videoServer getSurface chan = do
  image <- readChan chan
  surface <- getSurface image
  drawImage surface image
  let getSurface' image'
        | (width image, height image) /= (width image', height image') =
          newSurface image'
        | otherwise = return surface
  videoServer getSurface' chan

newSurface :: Image -> IO Surf
newSurface Image { width = width, height = height } = do
  screen <- setVideoMode width height 32 [SWSurface]
  rgb <- createRGBSurface [] width height 32 0xff0000 0xff00 0xff 0
  return (screen, rgb)

drawImage :: Surf -> Image -> IO ()
drawImage (screen, rgb) image = do
  buf <- fmap castPtr (surfaceGetPixels rgb)
  forM_ [0..height image*width image-1] $ \x ->
      pokeElemOff buf x (pixels image `unsafeAt` x)
  blitSurface rgb Nothing screen Nothing
  Graphics.UI.SDL.flip screen
  return ()

newVideoDriver :: IO VideoDriver
newVideoDriver = do
  chan <- newChan
  forkIO (videoServer newSurface chan)
  return chan

draw :: VideoDriver -> Image -> IO ()
draw = writeChan

-- testing
img = Image 640 480 arr
arr = array (0, 640*480-1) [(i, fromIntegral i) | i <- [0..640*480-1]]

img' = Image 640 480 arr'
arr' = array (0, 640*480-1) [(i, fromIntegral (640*480-1-i)) | i <- [0..640*480-1]]

go n vd = replicateM_ n (draw vd img >> draw vd img')
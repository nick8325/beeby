{-# LANGUAGE BangPatterns #-}
import Control.Monad hiding (forever)
import Six502.Simulator
import Six502
import Data.Array.IO
import qualified Data.ByteString as BS

main = do
  rom <- BS.readFile "OS12.ROM"
  basic <- BS.readFile "BASIC2.ROM"
  arr <- newArray (0, 0xffff) 0
  let blit str !ofs
        | BS.null str = return ()
        | otherwise = do
          writeArray arr ofs (fromIntegral (BS.head str))
          blit (BS.tail str) (ofs+1)
      -- dump = do
      --   x <- gets ticks
      --   when (x >= 100000000) $ liftIO $ error "100 million"
      dump = do
        s <- gets id
        liftIO (print s)
      loop = reset >> forever (dump >> cpu)
  blit basic 0x8000
  blit rom 0xc000
  putStrLn "go"
  run loop arr s0

-- Next steps:
-- change ST to IO monad
-- have some story for memory mapped i/o
-- finish something i can run

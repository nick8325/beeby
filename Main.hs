{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main(main) where

import Six502.Interpreter
import Six502.CPU
import Six502.System
import BBC.Machine
import BBC.Sheila
import Six502.Memory
import Control.Monad hiding (forever)
import Six502.Machine
import Codec.PPM
import Data.Bits

dumpScreen :: RAM -> IO ()
dumpScreen ram = do
  writePPM "bitmap" (320, 256) =<< pixels
  forM_ [0..24] $ \row -> do
    forM_ [0..39] $ \col -> do
      c <- peekDevice ram (0x7c00 + row*40 + col)
      putStr [toEnum (fromEnum c)]
    putStrLn ""
  where
    pixels = fmap (map toPixel) . sequence $
             [ pixel row col | row <- [0..255], col <- [0..319] ]
    toPixel True = (255,255,255)
    toPixel False = (0,0,0)
    pixel row col = do
      let colMaj = col `div` 8
          colMin = col `mod` 8
          rowMaj = row `div` 8
          rowMin = row `mod` 8
          idx = rowMaj * 320 + colMaj * 8 + rowMin
      val <- peekDevice ram (0x5800 + idx)
      return (testBit val (7 - colMin))

x `inRange` (y, z) = x >= y && x <= z

main = do
  machine@Machine{ram = ram} <- newMachine
  sheila <- newSheila machine
  system <- newSystem
  after system 100000000 $ do
    replicateM 10 $ do
      cpu
      gets id >>= liftIO . print
--    liftIO (dumpScreen ram)
    error "Finished!"
  run (reset >> execute system cpu) (Overlay sheila ram) s0

{-# LANGUAGE BangPatterns #-}
module Test(myCPU) where

import Control.Monad
import Six502
import Six502.Simulator

myCPU :: Int -> Step s ()
myCPU !n = replicateM_ n cpu

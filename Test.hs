{-# LANGUAGE BangPatterns #-}
module Test(myCPU) where

import Six502
import Six502.Simulator

myCPU :: Step s ()
myCPU = forever cpu

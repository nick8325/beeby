{-# LANGUAGE BangPatterns #-}
module Test(myCPU) where

import Six502
import Six502.Simulator

myCPU :: Int -> Step s
myCPU !n = Step (f n)
  where {-# INLINE f #-}
        f 0 !mem !state = return state
        f n mem state = run cpu mem state >>= f (n-1 :: Int) mem

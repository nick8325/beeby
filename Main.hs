{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main(main) where

import Six502.Interpreter
import Six502.CPU
import BBC.Machine

main = do
  machine <- newMachine
  run (reset >> forever cpu) machine s0

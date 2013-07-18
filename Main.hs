{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main(main) where

import Six502.Interpreter
import Six502.CPU
import BBC.Machine
import Six502.Memory
import Control.Monad hiding (forever)
import Six502.Machine

x `inRange` (y, z) = x >= y && x <= z

{-# INLINE hook #-}
hook ram = do
  pc <- liftM fromAddr loadPC
  when (pc `elem` [0xfff1, 0xfff4, 0xffee, 0xe0a4, 0xe0c1, 0xe0c3] ||
        pc `inRange` (0xdb76, 0xdb7f)) $
    gets id >>= liftIO . print

  n <- gets ticks
  when (n >= 1000000) $ do
    replicateM 10 $ do
      cpu
      gets id >>= liftIO . print
    liftIO (dumpScreen ram)
    error "Finished!"

main = do
  machine@(Overlay _ ram) <- newMachine
  run (reset >> forever (cpu >> hook ram)) machine s0

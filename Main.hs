{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main(main) where

import Six502.Interpreter
import Six502.CPU
import Six502.Machine
import BBC.Machine
import Six502.Memory
import Control.Monad hiding (forever)
import Numeric

hook ram = do
  pc <- liftM fromAddr loadPC
  let syscall f = liftM fromByte (peek (register A)) >>= liftIO . f
      osword 0 = putStrLn "read line"
      osbyte 0x83 = putStrLn "83"
      osbyte 0x84 = putStrLn "84"
      oswrch f = putStrLn $ "wrch " ++ showHex f ""
      oswrch2 _ = putStrLn "wrch2"
  case pc of
    0xfff1 -> syscall osword
    0xfff4 -> syscall osbyte
    0xffee -> syscall oswrch
    0xe0a4 -> liftM fromByte (peek (memory (address 0x27c))) >>= liftIO . oswrch
    0xe0c1 -> liftM fromByte (peek (memory (address 0x27c))) >>= liftIO . oswrch
    0xe0c3 -> syscall oswrch2
    _ -> return ()
  n <- gets ticks
  when (n >= 1000000) $ do
    liftIO (dumpScreen ram)
    liftM fromByte (peek (memory (address 0x27c))) >>= liftIO . print
    error "Finished!"

main = do
  machine@(Overlay _ ram) <- newMachine
  run (reset >> forever (cpu >> hook ram)) machine s0

module BBC.Sheila where

import Six502.Memory

data Sheila = Sheila

{-# NOINLINE myExpensiveThing #-}
myExpensiveThing :: () -> IO ()
myExpensiveThing () = return ()

instance IODevice Sheila where
  range _ = (0xfe00, 0xff00)
  peekDevice _ _ = myExpensiveThing () >> return 0
  pokeDevice _ _ _= myExpensiveThing () >> return ()


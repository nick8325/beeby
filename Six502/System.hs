-- Connect the 6502 up to a full computer. Provides support for
-- external interrupts and so on.

{-# LANGUAGE BangPatterns #-}
module Six502.System where

import Data.IORef
import Six502.Machine
import Six502.Interpreter
import Data.Int

type System mem = IORef (Queue mem)

-- Representation: the Int is a time *delta*:
-- how long after the previous event this event fires.
-- The event handler gets given the current absolute time as a parameter
-- (used in after_).
data Queue mem = After {-# UNPACK #-} !Int64 (Int64 -> Step mem ()) (Queue mem)

{-# INLINE nil #-}
nil :: Queue mem
nil = aux
  where
    -- Do nothing, every so often :)
    aux = After maxBound (\_ -> return ()) aux

insert :: Int64 -> Step mem () -> Queue mem -> Queue mem
insert time act (After time' act' q)
  | time < time' =
    After time (\_ -> act) (After (time' - time) act' q)
  | otherwise =
    After time' act' (insert (time - time') act q)

atomicModifyIORef'' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'' ref f =
  atomicModifyIORef' ref (\x -> (f x, ()))

-- Schedule an event to happen when the *absolute time*
-- reaches a certain value.
after :: System mem -> Int64 -> Step mem () -> IO ()
after ref time act =
  -- Problem: we don't know the current time, and don't
  -- want to force the step code to maintain it in an IORef.
  -- Solution: add an event *now*.
  -- That event receives the current time, and its handler will
  -- schedule the real event.
  atomicModifyIORef'' ref $
    After 0 $ \now ->
      liftIO $
        atomicModifyIORef'' ref $
          insert ((time - now) `max` 0) act

{-# INLINE execute #-}
-- Run the system.
execute :: System mem -> Step mem () -> Step mem ()
execute ref cpu = do
  n <- currentTicks
  forever aux n
  where
    aux !n0 = do
      !n1 <- currentTicks
      After delta _ _ <- liftIO (readIORef ref)
      if n0 + delta <= n1 then do
        After delta act _ <- liftIO (atomicModifyIORef' ref (\q@(After _ _ q') -> (q', q)))
        act n1
        return (n0 + delta)
       else do
        cpu
        return n0

{-# INLINE newSystem #-}
newSystem :: IO (System mem)
newSystem = newIORef nil
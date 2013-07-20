-- Connect the 6502 up to a full computer. Provides support for
-- external interrupts and so on.

{-# LANGUAGE BangPatterns #-}
module Six502.System where

import Data.IORef
import Control.Monad
import Six502.Machine
import Six502.Interpreter
import Data.Int
import Data.Word
import Graphics.UI.SDL.Time

type System mem a = IORef (Queue mem a)

-- Representation: the Int is a time *delta*:
-- how long after the previous event this event fires.
data Queue mem a = After {-# UNPACK #-} !Int64 (Step mem (Maybe a)) (Queue mem a)

{-# INLINE nil #-}
nil :: Queue mem a
nil = aux
  where
    -- Do nothing, every so often :)
    aux = After maxBound (return Nothing) aux

insert :: Int64 -> Step mem (Maybe a) -> Queue mem a -> Queue mem a
insert time act (After time' act' q)
  | time < time' =
    After time act (After (time' - time) act' q)
  | otherwise =
    After time' act' (insert (time - time') act q)

atomicModifyIORef'' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'' ref f =
  atomicModifyIORef' ref (\x -> (f x, ()))

-- Interrupt the processor to perform an action.
interrupt :: System mem a -> Step mem (Maybe a) -> IO ()
interrupt ref act = atomicModifyIORef'' ref (After 0 act)

-- Schedule an event to happen when the *absolute time*
-- reaches a certain value.
after :: System mem a -> Int64 -> Step mem (Maybe a) -> IO ()
after ref time act =
  -- Interrupt the processor to find the current time,
  -- so that we know when to schedule the event.
  interrupt ref $ do
    now <- currentTicks
    liftIO $ atomicModifyIORef'' ref $
      insert ((time - now) `max` 0) act
    return Nothing

-- Schedule a regular event.
every :: System mem a -> Int64 -> Step mem (Maybe a) -> IO ()
every ref delta act = interrupt ref reschedule
  where
    reschedule = do
      time <- currentTicks
      liftIO (after ref (time + delta) (reschedule >> act))
      return Nothing

waitUntil :: Word32 -> IO ()
waitUntil t = do
  t0 <- getTicks
  -- handle wraparound
  when (t - t0 < 0x80000000) (delay (t - t0))

{-# INLINE execute #-}
-- Run the system.
execute :: System mem a -> Step mem () -> Step mem a
execute ref cpu = do
  n <- currentTicks
  t <- liftIO getTicks
  let
    aux !n0 = do
      !n1 <- currentTicks
      After delta _ _ <- liftIO (readIORef ref)
      if n0 + delta <= n1 then do
        After delta act _ <- liftIO (atomicModifyIORef' ref (\q@(After _ _ q') -> (q', q)))
        liftIO (waitUntil (t + fromIntegral ((n0 + delta - n) `div` 2000)))
        x <- act
        case x of
          Nothing -> return (Left (n0 + delta))
          Just y -> return (Right y)
       else do
        cpu
        return (Left n0)
  loop aux n

{-# INLINE newSystem #-}
newSystem :: IO (System mem a)
newSystem = newIORef nil
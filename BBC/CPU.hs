-- Simulate a 6502 in the "real world".
-- Takes the pure 6502 simulator, and adds asynchronous interrupts,
-- timed events (e.g. screen refresh) and real time.

{-# LANGUAGE BangPatterns #-}
module BBC.CPU where

import Data.IORef
import Control.Monad
import Six502.Machine
import Six502.Interpreter
import Data.Int
import Data.Word
import Graphics.UI.SDL.Time

clockSpeed :: Int64
clockSpeed = 2000000

ticksToMs, msToTicks :: Int64 -> Int64
ticksToMs n = n `div` 2000
msToTicks n = n * 2000

type CPU mem a = IORef (Queue mem a)
-- An event can return a Just to stop the simulation.
type Event mem a = Step mem (Maybe a)

-- A queue of events that will occur in the future.
-- The Int64 is a time *delta*:
-- how many ticks after the previous event this event fires.
data Queue mem a = After {-# UNPACK #-} !Int64 (Event mem a) (Queue mem a)

{-# INLINE nil #-}
nil :: Queue mem a
nil = aux
  where
    -- Do nothing, every so often :)
    aux = After maxBound (return Nothing) aux

insert :: Int64 -> Event mem a -> Queue mem a -> Queue mem a
insert time act (After time' act' q)
  | time < time' =
    After time act (After (time' - time) act' q)
  | otherwise =
    After time' act' (insert (time - time') act q)

atomicModifyIORef'' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'' ref f =
  atomicModifyIORef' ref (\x -> (f x, ()))

-- Interrupt the processor to perform an action.
interrupt :: CPU mem a -> Event mem a -> IO ()
interrupt ref act = atomicModifyIORef'' ref (After 0 act)

-- Schedule an event to happen when the *absolute time*
-- (measured in clock ticks) reaches a certain value.
after :: CPU mem a -> Int64 -> Event mem a -> IO ()
after ref time act =
  -- Interrupt the processor to find the current time,
  -- so that we know when to schedule the event.
  interrupt ref $ do
    now <- currentTicks
    liftIO $ atomicModifyIORef'' ref $
      insert ((time - now) `max` 0) act
    return Nothing

-- Schedule a regular event.
every :: CPU mem a -> Int64 -> Event mem a -> IO ()
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

{-# INLINE runCPU #-}
-- Run the processor.
runCPU :: CPU mem a -> Step mem () -> Step mem a
runCPU ref cpu = do
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

{-# INLINE newCPU #-}
newCPU :: IO (CPU mem a)
newCPU = newIORef nil
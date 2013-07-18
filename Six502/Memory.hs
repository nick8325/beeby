-- Memory-mapped I/O devices.

{-# LANGUAGE BangPatterns, FlexibleInstances, TypeFamilies, MultiParamTypeClasses #-}
module Six502.Memory where

import Six502.Machine
import Data.Word
import qualified Data.ByteString as BS

-- A machine that can do memory-mapped I/O.
class MemorylessMachine m => IOMachine m where
  peekIO :: (Int -> IO Word8) -> Addr m -> m (Byte m)
  pokeIO :: (Int -> Word8 -> IO ()) -> Addr m -> Byte m -> m ()

-- A memory-mapped I/O device.
class IODevice a where
  -- The range of addresses taken by the device.
  range :: a -> (Int, Int)
  -- Peek or poke the I/O device.
  peekDevice :: a -> Int -> IO Word8
  pokeDevice :: a -> Int -> Word8 -> IO ()

-- An address space.
class MemorylessMachine m => AddressSpace m a where
  fetchAddress :: a -> Addr m -> m (Byte m)
  fetchAddress = peekAddress
  peekAddress :: a -> Addr m -> m (Byte m)
  pokeAddress :: a -> Addr m -> Byte m -> m ()

-- An I/O device overlayed on top of an address space.
data Overlay a b = Overlay a b

instance (IODevice a, IOMachine m, AddressSpace m b) => AddressSpace m (Overlay a b) where
  {-# INLINE fetchAddress #-}
  fetchAddress (Overlay _ mem) = fetchAddress mem
  {-# INLINE[0] peekAddress #-}
  peekAddress (Overlay dev mem) !addr =
    condRange addr (range dev)
      (peekIO (peekDevice dev) addr)
      (peekAddress mem addr)
  {-# INLINE[0] pokeAddress #-}
  pokeAddress (Overlay dev mem) !addr !v =
    condRange addr (range dev)
      (pokeIO (pokeDevice dev) addr v)
      (pokeAddress mem addr v)

-- Utilities for writing to memory.

{-# INLINE blit #-}
blit :: IODevice a => a -> Int -> BS.ByteString -> IO ()
blit mem = aux
  where
    aux !ofs str
      | BS.null str = return ()
      | otherwise = do
        pokeDevice mem ofs (fromIntegral (BS.head str) :: Word8)
        aux (ofs+1) (BS.tail str)

{-# INLINE fill #-}
fill :: IODevice a => a -> Int -> Int -> Word8 -> IO ()
fill mem from to w = blit mem from (BS.replicate (to - from) w)

-- Memory-mapped I/O devices.

{-# LANGUAGE BangPatterns, FlexibleInstances, TypeFamilies, MultiParamTypeClasses #-}
module Six502.Memory where

import Six502.Machine
import Data.Word

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
  fetchAddress (Overlay _ mem) = fetchAddress mem
  peekAddress (Overlay dev mem) !addr =
    condRange addr (range dev)
      (peekIO (peekDevice dev) addr)
      (peekAddress mem addr)
  pokeAddress (Overlay dev mem) !addr !v =
    condRange addr (range dev)
      (pokeIO (pokeDevice dev) addr v)
      (pokeAddress mem addr v)
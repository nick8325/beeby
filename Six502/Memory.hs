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
class IOMachine m => IODevice m a where
  -- Compute whether a particular address is mapped to the I/O device.
  visible :: a -> Addr m -> Bit m
  -- Peek or poke the I/O device.
  peekDevice :: a -> Addr m -> m (Byte m)
  pokeDevice :: a -> Addr m -> Byte m -> m ()

-- An address space.
class MemorylessMachine m => AddressSpace m a where
  fetchAddress :: a -> Addr m -> m (Byte m)
  fetchAddress = peekAddress
  peekAddress :: a -> Addr m -> m (Byte m)
  pokeAddress :: a -> Addr m -> Byte m -> m ()

-- An I/O device overlayed on top of an address space.
data Overlay a b = Overlay !a !b

instance (IODevice m a, IOMachine m, AddressSpace m b) => AddressSpace m (Overlay a b) where
  fetchAddress (Overlay _ mem) = fetchAddress mem
  peekAddress (Overlay dev mem) !addr =
    cond (visible dev addr)
      (peekDevice dev addr)
      (peekAddress mem addr)
  pokeAddress (Overlay dev mem) !addr !v =
    cond (visible dev addr)
      (pokeDevice dev addr v)
      (pokeAddress mem addr v)
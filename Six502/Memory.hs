-- A module for describing memory hierarchies in a backend-independent way.

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Six502.Memory where

import Six502
import Data.Word

-- A class of machines that implement a memory.
class MachineMemory (m :: * -> *) where
  -- Memories are constructed in three ways:
  --   * RAM is a 64KB ordinary memory
  --   * Any memory of class IOMemory is a memory
  --     (this is typically for memory-mapped I/O)
  --   * Memories can be combined with Overlay
  type RAM m
  type Overlay m :: * -> * -> *

-- Memory-mapped I/O.
class IOMemory mem where
  visible :: mem -> Int -> Bool
  fetchMemory :: mem -> Int -> IO Word8
  fetchMemory = peekMemory
  peekMemory :: mem -> Int -> IO Word8
  pokeMemory :: mem -> Int -> Word8 -> IO ()

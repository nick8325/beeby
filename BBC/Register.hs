-- Hardware register support.
module BBC.Register where

import Data.IORef
import Text.Printf

data Register a = Register {
  readRegister :: IO a,
  writeRegister :: a -> IO ()
  }

register :: a -> IO (Register a)
register x = do
  r <- newIORef x
  return Register {
    readRegister = readIORef r,
    writeRegister = writeIORef r
    }

switch :: a -> (a -> Register b) -> IO (Register a, Register b)
switch def select = do
  reg <- register def
  let read = readRegister reg >>= readRegister . select
      write val = readRegister reg >>= flip writeRegister val . select
  return (reg, Register read write)

afterWrite :: Register a -> (a -> IO ()) -> Register a
afterWrite reg act = reg { writeRegister = \val -> writeRegister reg val >> act val }

constRegister :: a -> Register a
constRegister def = Register { readRegister = return def, writeRegister = const (return ()) }

activeRegister :: a -> (a -> IO ()) -> IO (Register a)
activeRegister def act = do
  act def
  reg <- register def
  return (reg `afterWrite` act)

writeOnlyRegister :: a -> (a -> IO ()) -> Register a
writeOnlyRegister def act =
  constRegister def `afterWrite` act

unknownRegister xs addr =
  writeOnlyRegister 0 $ \val ->
    printf "Write of %02x to unknown %s %04x\n" val xs addr

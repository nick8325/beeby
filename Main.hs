{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main(main) where

import Control.Monad hiding (forever)
import Six502.Machine
import Six502.Interpreter
import Six502.Memory
import Six502.CPU
import qualified Data.ByteString as BS
import System.IO
import Data.Word
import Data.Bits hiding (bit, xor)
import qualified Data.Bits
import Numeric
import Data.Primitive.ByteArray

data Sheila = Sheila

{-# NOINLINE myExpensiveThing #-}
myExpensiveThing :: () -> IO ()
myExpensiveThing () = return ()

instance IOMachine m => IODevice m Sheila where
  visible _ addr = page addr `eq` byte 0xfe
  peekDevice _ = peekIO (\_ -> myExpensiveThing () >> return 0)
  pokeDevice _ = pokeIO (\_ _ -> myExpensiveThing () >> return ())

type Mem = Overlay Sheila RAM

main = do
  rom <- BS.readFile "OS12.ROM"
  basic <- BS.readFile "BASIC2.ROM"
  arr <- newRAM
  let blit str !ofs
        | BS.null str = return ()
        | otherwise = do
          writeByteArray arr ofs (fromIntegral (BS.head str) :: Word8)
          blit (BS.tail str) (ofs+1)
      fill c from to = blit (BS.replicate (to - from) c) from
      -- dump = return ()
      {-# INLINE syscall #-}
      syscall f = liftM fromByte (peek (register A)) >>= f
      dump = do
        x <- gets ticks
        when (x >= 100000000) $ liftIO $ error "100 million"
      -- dump = do
      --   s <- gets id
      --   liftIO (print s)
      osword 0 = do
        x <- peek (register X)
        y <- peek (register Y)
        let base = paged y x
        buf <- liftM fromAddr (peek16 base)
        len <- liftM fromByte (peek (memory (base `index` byte 2)))
        minVal <- liftM fromByte (peek (memory (base `index` byte 3)))
        maxVal <- liftM fromByte (peek (memory (base `index` byte 4)))
        str <- liftIO BS.getLine
        let inBounds c = fromIntegral c >= minVal && fromIntegral c <= maxVal
            res = BS.take (len-1) (BS.takeWhile inBounds str) `BS.snoc` 13
        poke (register Y) (byte (BS.length res))
        setFlag Carry (bit False)
        liftIO (blit res buf)
        -- liftIO (print (fromAddr base, buf, len, minVal, maxVal, str, res))
        rts
      osbyte 0x83 = poke (register X) (byte 0) >> poke (register Y) (byte 0xe) >> rts
      osbyte 0x84 = poke (register X) (byte 0) >> poke (register Y) (byte 0x80) >> rts
      oswrch c = liftIO (putChar (toEnum c) >> hFlush stdout) >> rts
      -- {-# INLINE hook #-}
      -- hook = do
      --   pc <- liftM fromAddr loadPC
      --   case pc of
      --     0xfff1 -> syscall osword
      --     0xfff4 -> syscall osbyte
      --     0xffee -> syscall oswrch
      --     0xe0a4 -> syscall oswrch
      --     _ -> return ()
      loop :: Step Mem ()
      loop = reset >> forever (dump >> cpu)
  blit basic 0x8000
  blit rom 0xc000
  fill 0xff 0xfc00 0xfeff
  fill 0x00 0xfe40 0xfe4f
  putStrLn "go"
  run loop (Overlay Sheila arr) s0

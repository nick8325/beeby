{-# LANGUAGE BangPatterns #-}
import Control.Monad hiding (forever)
import Six502.Simulator
import Six502
import Data.Array.IO hiding (index)
import qualified Data.ByteString as BS
import System.IO

main = do
  rom <- BS.readFile "OS12.ROM"
  basic <- BS.readFile "BASIC2.ROM"
  arr <- newArray (0, 0xffff) 0
  let blit str !ofs
        | BS.null str = return ()
        | otherwise = do
          writeArray arr ofs (fromIntegral (BS.head str))
          blit (BS.tail str) (ofs+1)
      fill c from to = blit (BS.replicate (to - from) c) from
      dump = return ()
      {-# INLINE syscall #-}
      syscall f = liftM fromByte (peek (register A)) >>= f
      -- dump = do
      --   x <- gets ticks
      --   when (x >= 100000000) $ liftIO $ error "100 million"
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
      {-# INLINE hook #-}
      hook = do
        pc <- liftM fromAddr loadPC
        case pc of
          0xfff1 -> syscall osword
          0xfff4 -> syscall osbyte
          0xffee -> syscall oswrch
          0xe0a4 -> syscall oswrch
          _ -> return ()
      loop = reset >> forever (dump >> hook >> cpu)
  blit basic 0x8000
  blit rom 0xc000
  fill 0xff 0xfc00 0xfeff
  fill 0x00 0xfe40 0xfe4f
  putStrLn "go"
  run loop arr s0

-- Next steps:
-- change ST to IO monad
-- have some story for memory mapped i/o
-- finish something i can run

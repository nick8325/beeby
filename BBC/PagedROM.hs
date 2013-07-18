-- Support for loading paged ROMs.
module BBC.PagedROM where

import qualified Data.ByteString as BS
import Six502.Interpreter
import BBC.Register
import Data.Word
import Six502.Memory

newPagedROM :: RAM -> (Word8 -> BS.ByteString) -> IO (Register Word8)
newPagedROM ram select =
  activeRegister 0 $ \bank -> do
    putStrLn $ "Load paged ROM " ++ show bank
    blit ram 0x8000 (select bank)

newSinglePagedROM :: RAM -> BS.ByteString -> IO (Register Word8)
newSinglePagedROM ram rom = newPagedROM ram select
  where
    select 15 = rom
    select _ = BS.replicate 0x4000 0xff
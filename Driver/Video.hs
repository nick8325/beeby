-- Video output.
module Driver.Video where

type VideoDriver = ()

newVideoDriver :: IO VideoDriver
newVideoDriver = return ()
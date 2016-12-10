module Data.Wavelet.Internal.Buffer where

import Data.Wavelet.Internal.Input
import Data.Wavelet.Internal.Shared
import Data.Wavelet.Internal.Types

import Data.Vector.Storable             (Storable, copy)
import Data.Vector.Storable.Mutable     (IOVector)
import Data.Vector.Storable.MMap
import System.Directory                 (makeAbsolute, createDirectoryIfMissing)

data Buffer a = Buffer { getBufferPath :: FilePath
                       , getBuffer :: IOVector a }

usingBuffer :: Storable a => Input a -> IndexPath -> (Buffer a -> IO b) -> IO b
usingBuffer input indexPath f = do

    createDirectoryIfMissing True indexPath

    bufferPath <- makeAbsolute $ concat [indexPath, "/", "buf.tmp"]
    removeFileIfExists bufferPath

    buf <- unsafeMMapMVector bufferPath ReadWriteEx (Just (0, getInputLength input))
    copy buf (getInput input)

    x <- f Buffer { getBufferPath = bufferPath
                  , getBuffer = buf }
    removeFileIfExists bufferPath
    return x

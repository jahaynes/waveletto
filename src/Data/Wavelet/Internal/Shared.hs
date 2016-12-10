{-# LANGUAGE BangPatterns #-}

module Data.Wavelet.Internal.Shared where

import Prelude hiding (null, splitAt)

import Control.Exception            (catch, throwIO)
import Data.Vector.Storable         (Vector, Storable, splitAt, null) 
import Data.Vector.Storable.Mutable (IOVector)
import Data.Vector.Storable.MMap    (unsafeMMapMVector, Mode(ReadWriteEx))
import System.Directory             (removeFile, removeDirectoryRecursive)
import System.IO.Error              (isDoesNotExistError)

{- Int division (rounding up) -}
quot1 :: Int -> Int -> Int
quot1 a b =
    case a `quotRem` b of
        (x,0) -> x
        (x,_) -> x + 1

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
    where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise = throwIO e

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists fileName = removeDirectoryRecursive fileName `catch` handleExists
    where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise = throwIO e

recreateFileWithSize :: Storable a => FilePath -> Int -> IO (IOVector a)
recreateFileWithSize fp fsize = do
    removeFileIfExists fp
    unsafeMMapMVector fp ReadWriteEx (Just (0, fsize))

chunksOf :: Storable a => Int -> Vector a -> [Vector a]
chunksOf n vec | null vec = []
               | otherwise =
                   let (some, rest) = splitAt n vec
                   in some : chunksOf n rest

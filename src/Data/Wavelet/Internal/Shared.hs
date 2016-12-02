{-# LANGUAGE BangPatterns #-}

module Data.Wavelet.Internal.Shared where

import Prelude hiding (null, splitAt)

import Control.Exception        (catch, throwIO)
import Data.Bits                 
import Data.Vector.Storable     (Vector, Storable, splitAt, null, (!)) 
import Data.Word                (Word64)
import System.Directory         (removeFile, removeDirectoryRecursive)
import System.IO.Error          (isDoesNotExistError)

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

chunksOf :: Storable a => Int -> Vector a -> [Vector a]
chunksOf n vec | null vec = []
               | otherwise =
                   let (some, rest) = splitAt n vec
                   in some : chunksOf n rest
                   
countNBitsOf :: Vector Word64 -> Int -> Int
countNBitsOf vec = go 0 0
    where
    go !acc i        0 = acc
    go  acc i bitsLeft
        | bitsLeft >= 64 =
            let count = popCount (vec ! i)
            in
            go (acc+count) (i+1) (bitsLeft - 64)
        | otherwise =
            let mask = (1 `shiftL` bitsLeft) - 1
            in acc + popCount (mask .&. (vec ! i))

module Data.Wavelet.Internal.Shared where

import System.Directory         (removeFile)
import Control.Exception        (catch, throwIO)
import System.IO.Error          (isDoesNotExistError)

{- Int division (rounding up) -}
quot1 :: Int -> Int -> Int
quot1 a b =
    case a `quotRem` b of
        (x,0) -> x
        (x,_) -> x + 1

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise = throwIO e

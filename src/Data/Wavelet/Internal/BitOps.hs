module Data.Wavelet.Internal.BitOps where

import Data.Bits
import           Data.Vector.Storable       (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word                            (Word64)

{- Count the number of 1-bits in vector of word64s, in the bit range x[0..n] -}
rnk1Vector64 :: Int -> Vector Word64 -> Int
rnk1Vector64 n vec =
        let (wordsToCheck, bitsToCheck) = n `divMod` 64
            wordParts = VS.sum . VS.map popCount . VS.take wordsToCheck $ vec
            bitParts = rnk1Word64 bitsToCheck . VS.head . VS.drop wordsToCheck $ vec
        in wordParts + bitParts

{- Count the number of 1-bits in one word64, in the bit range x[0..n] -}
rnk1Word64 :: Int -> Word64 -> Int
rnk1Word64 n x =
    let mask = (1 `shiftL` n) - 1
    in popCount (mask .&. x)
{-# INLINE rnk1Word64 #-}

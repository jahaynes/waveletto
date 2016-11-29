module Data.Wavelet.Internal.Input where

import           Data.Bits                          (FiniteBits, countLeadingZeros, finiteBitSize)
import           Data.Vector.Storable               (Vector, Storable)
import qualified Data.Vector.Storable as VS         (length, maximum)

data Input a = Input { getInput :: Vector a
                     , getRequiredLayers :: Int
                     , getInputLength :: Int
                     } deriving Show

prepareInput :: (Show a, FiniteBits a, Ord a, Num a, Storable a) => Vector a -> Input a
prepareInput vec =

    let bitsPerElement = finiteBitSize maximumInputSymbol
        leadingZeros = countLeadingZeros maximumInputSymbol
        numRequiredLayers = bitsPerElement - leadingZeros

    in Input { getInput = vec
             , getRequiredLayers = numRequiredLayers
             , getInputLength = VS.length vec
             }

    where
    maximumInputSymbol = VS.maximum vec

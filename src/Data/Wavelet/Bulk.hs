module Data.Wavelet.Bulk where

import Data.Wavelet.Internal.Types  (Position)

import Data.Bits                    (Bits)
import Data.Vector.Storable         (Vector, Storable)

class BulkWavelet structure where

    {- Return many elements when given many positions -}
    accessMany :: (Bits a, Storable a) => structure -> Vector Position -> Vector a

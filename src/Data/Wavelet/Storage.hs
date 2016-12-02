module Data.Wavelet.Storage where

import Data.Wavelet.Internal.Input  (Input)
import Data.Wavelet.Internal.Types  (IndexPath)

import Data.Bits                    (Bits)
import Data.Vector.Storable         (Storable)

class FromDirectory structure where

    {- Load this structure from a directory -}
    load :: IndexPath -> IO structure

    {- Create and return a new structure in this directory from a given vector -}
    create :: (Bits a, Ord a, Storable a) => IndexPath -> Input a -> IO structure

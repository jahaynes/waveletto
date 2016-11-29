{-# LANGUAGE InstanceSigs #-}

module Data.Wavelet.Auxiliary.RankBlocks where

import Data.Wavelet.Storage         (FromDirectory (..))
import Data.Wavelet.Internal.Input  (Input)
import Data.Wavelet.Internal.Types  (IndexPath)

import Data.Bits                    (Bits)
import Data.Vector.Storable         (Storable)

data RankBlocks = RankBlocks deriving Show


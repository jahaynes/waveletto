{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Wavelet.Internal.Types where

type Count = Int

type Position = Int

type RequiredLayers = Int

type IndexPath = FilePath 

newtype LayerNum = LayerNum Int deriving Enum


{-# LANGUAGE InstanceSigs #-}

module Data.Wavelet.Auxiliary.RankBlocks where

import Data.Wavelet.Internal.Shared
import Data.Wavelet.Internal.Types
import Data.Wavelet.Matrix.Geometry

import Control.Monad                                    (foldM_)
import Data.Bits                                        (popCount)
import Data.Word                                        (Word64)
import           Data.Vector.Storable                   (Vector)
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable   as VM
import           Data.Vector.Storable.MMap
import System.Directory                                 (makeAbsolute)

data RankBlocks = RankBlocks Int (Vector Int) deriving Show

type NumberOfWords = Int

blockSize :: NumberOfWords
blockSize = 64  --TODO investigate this size

createRankBlocks :: IndexPath -> Vector Word64 -> Geometry -> IO RankBlocks
createRankBlocks indexPath payload geometry = do

    let totalBits = getInputLength geometry
        bitsPerLayer = totalBits `quot1` blockSize
        numLayers = getNumLayers geometry
        totalSize = bitsPerLayer * numLayers
        w64sPerLayer = getW64sPerLayer geometry
        layers = chunksOf w64sPerLayer payload

    vm <- do
        let fileSize = numLayers * (w64sPerLayer `quot` blockSize)
        rankPath <- getRankPath indexPath
        removeFileIfExists rankPath
        print ("filesize", fileSize)
        unsafeMMapMVector rankPath ReadWriteEx (Just (0, fileSize)) :: IO (VM.IOVector Int)

    let numW64sInEachRankLayer = (w64sPerLayer `quot1` blockSize) - 1
    processLayer vm 0 layers
    RankBlocks numW64sInEachRankLayer <$> VS.unsafeFreeze vm

    where
    processLayer :: VM.IOVector Int -> Int -> [Vector Word64] -> IO ()
    processLayer vm outPtr         [] = print ("outPtr", outPtr)
    processLayer vm outPtr (layer:ls) = do

        let payLoadBlocks = chunksOf blockSize layer
            mostPayloadBlocks = init payLoadBlocks
            bitCountMost = map (VS.sum . VS.map popCount) mostPayloadBlocks

        foldM_ (\acc (o,bc) -> do
                    let acc' = acc + bc
                    VM.write vm o acc'
                    return acc') 0 (zip [outPtr..] bitCountMost)

        processLayer vm (outPtr + length mostPayloadBlocks) ls

loadRankBlocks :: IndexPath -> Geometry -> IO RankBlocks
loadRankBlocks indexPath geometry = do
    rankPath <- getRankPath indexPath
    RankBlocks (deriveRankBlockLayerSize geometry) <$> unsafeMMapVector rankPath Nothing

deriveRankBlockLayerSize :: Geometry -> Int
deriveRankBlockLayerSize geometry =
    (getW64sPerLayer geometry `quot1` blockSize) - 1

getRankPath :: IndexPath -> IO FilePath
getRankPath indexPath = makeAbsolute (indexPath ++ "/rank_blocks")

down :: RankBlocks -> RankBlocks
down (RankBlocks layerSize vec) = RankBlocks layerSize (VS.drop layerSize vec)

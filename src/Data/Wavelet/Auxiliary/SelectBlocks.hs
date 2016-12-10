{-# LANGUAGE BangPatterns #-}

module Data.Wavelet.Auxiliary.SelectBlocks where

import Data.Wavelet.Internal.Shared
import Data.Wavelet.Internal.Types
import Data.Wavelet.Matrix.Geometry

import Data.Bits                                        (testBit)
import Data.Word                                        (Word64)
import           Data.Vector.Storable                   (Vector, (!))
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable   as VM
import           Data.Vector.Storable.MMap
import System.Directory                                 (makeAbsolute)

import Control.Monad    (zipWithM_)

data Select0Blocks = Select0Blocks !Int !(Vector Int)
data Select1Blocks = Select1Blocks !Int !(Vector Int)

type NumberOfBits = Int

data Chunk = ZeroChunk !Int
           | OneChunk  !Int

blockSize :: NumberOfBits
blockSize = 256 --TODO investigate this size

createSelectBlocks :: IndexPath -> Vector Word64 -> Geometry -> IO (Select0Blocks, Select1Blocks)
createSelectBlocks indexPath payload geometry = do

    let inputTotalBits = getInputLength geometry
        layers = chunksOf (getW64sPerLayer geometry) payload
        calculatedLayers = map (calculateLayers inputTotalBits) layers        

        lookupsPerLayer = calculateNumLookupsPerLayer inputTotalBits

        numLayers = getNumLayers geometry
        
        lookupsPerFile = numLayers * lookupsPerLayer

    select0Path <- getSelect0Path indexPath
    vm0 <- recreateFileWithSize select0Path lookupsPerFile

    select1Path <- getSelect1Path indexPath
    vm1 <- recreateFileWithSize select1Path lookupsPerFile

    let fileOffsets = iterate (+lookupsPerLayer) 0

    zipWithM_
        (processLayer vm0 vm1)
        fileOffsets
        calculatedLayers

    se0 <- Select0Blocks lookupsPerLayer <$> VS.unsafeFreeze vm0
    se1 <- Select1Blocks lookupsPerLayer <$> VS.unsafeFreeze vm1

    return (se0, se1)

    where
    processLayer vm0 vm1 fileOffset layer = do

        --Each layer starts with a 0
        VM.write vm0 fileOffset 0
        VM.write vm1 fileOffset 0

        go 1 1 layer

        where
        go _ _ [] = return ()

        go i j (ZeroChunk x:cs) = do
            VM.write vm0 (fileOffset+i) x
            go (i+1) j cs

        go i j (OneChunk x:cs) = do
            VM.write vm1 (fileOffset+j) x
            go i (j+1) cs

{- Each select structure will have this many lookups per layer
   (maximum) The +1 is for a leading 0 -}
calculateNumLookupsPerLayer :: Int -> Int
calculateNumLookupsPerLayer inputLength = 1 + (inputLength `quot` blockSize)

calculateLayers :: Int -> Vector Word64 -> [Chunk]
calculateLayers totalBits layer = go totalBits 0 0 0 0

    where
    go :: Int -> Int -> Int -> Int -> Int -> [Chunk]
    go  0    _    _  _  _ = []
    go br acc0 acc1 wi 64 = go br acc0 acc1 (wi+1) 0
    go br acc0 acc1 wi bi
        | testBit (layer ! wi) bi =
            if acc1+1 == blockSize
                then  OneChunk (wi * 64 + bi + 1) : go (br-1)     acc0        0 wi (bi+1)
                else                                go (br-1)     acc0 (acc1+1) wi (bi+1)
        | otherwise =
            if acc0+1 == blockSize
                then ZeroChunk (wi * 64 + bi + 1) : go (br-1)        0 acc1     wi (bi+1)
                else                                go (br-1) (acc0+1) acc1     wi (bi+1)

loadSelectBlocks :: IndexPath -> Geometry -> IO (Select0Blocks, Select1Blocks)
loadSelectBlocks indexPath geometry = do

    select0Path <- getSelect0Path indexPath
    select1Path <- getSelect1Path indexPath

    let lookupsPerLayer = calculateNumLookupsPerLayer (getInputLength geometry)

    se0 <- Select0Blocks lookupsPerLayer <$> unsafeMMapVector select0Path Nothing
    se1 <- Select1Blocks lookupsPerLayer <$> unsafeMMapVector select1Path Nothing 

    return (se0, se1)

--deriveRankBlockLayerSize :: Geometry -> Int
--deriveRankBlockLayerSize geometry =
    --(getW64sPerLayer geometry `quot1` blockSize) - 1

select1WithLookup :: Vector Word64 -> Select1Blocks -> Int -> Int              
select1WithLookup payload (Select1Blocks _ se1vec) n__ =

    let (a,b) = n__ `quotRem` blockSize
        precounted = se1vec ! a
        c = count1s precounted b
    in
    precounted + c

    where
    count1s :: Int -> Int -> Int
    count1s skip n_ = do
        let (dropBlocks, dropBits) = skip `divMod` 64
        go1 0 dropBlocks dropBits n_

        where
        go1 !acc wi 64 n = go1 acc (wi+1) 0 n
        go1  acc wi bi n
            | testBit (payload ! wi) bi =
                if n-1 < 0
                    then acc
                    else go1 (acc+1) wi (bi+1) (n-1)
            | otherwise = go1 (acc+1) wi (bi+1) n

select0WithLookup :: Vector Word64 -> Select0Blocks -> Int -> Int              
select0WithLookup payload (Select0Blocks _ se0vec) n__ =

    let (a,b) = n__ `quotRem` blockSize
        precounted = se0vec ! a
        c = count0s precounted b
    in
    precounted + c

    where
    count0s :: Int -> Int -> Int
    count0s skip n_ = do
        let (dropBlocks, dropBits) = skip `divMod` 64
        go0 0 dropBlocks dropBits n_

        where
        go0 !acc wi 64 n = go0 acc (wi+1) 0 n
        go0  acc wi bi n
            | testBit (payload ! wi) bi = go0 (acc+1) wi (bi+1) n
            | otherwise =
                if n-1 < 0
                    then acc
                    else go0 (acc+1) wi (bi+1) (n-1)    

getSelect0Path :: IndexPath -> IO FilePath
getSelect0Path indexPath = makeAbsolute (indexPath ++ "/select0_blocks")

getSelect1Path :: IndexPath -> IO FilePath
getSelect1Path indexPath = makeAbsolute (indexPath ++ "/select1_blocks")

down0 :: Select0Blocks -> Select0Blocks
down0 (Select0Blocks layerSize vec) = Select0Blocks layerSize (VS.drop layerSize vec)

down1 :: Select1Blocks -> Select1Blocks
down1 (Select1Blocks layerSize vec) = Select1Blocks layerSize (VS.drop layerSize vec)

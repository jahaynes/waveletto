{-# LANGUAGE BangPatterns, TypeFamilies #-}

module Data.Wavelet.Internal.Partition where

import Data.Wavelet.Internal.Types

import Data.Vector.Storable.Mutable as VM
import Data.Vector.Storable.MMap
import Data.Bits                      (Bits, testBit)

inPlaceStableSortByBitN :: (Bits a, Storable a) => IOVector a -> FilePath -> LayerNum -> IO ()
inPlaceStableSortByBitN inputVec tempBufferPath (LayerNum layerNum) = do

    let len = VM.length inputVec
    numOnes <- countNumOnes inputVec
    let numZeroes = len - numOnes

    mOut <- unsafeMMapMVector
               tempBufferPath
               ReadWriteEx
               (Just (0,len))

    go 0 (VM.length inputVec) mOut 0 numZeroes
    VM.copy inputVec mOut

    where
    countNumOnes :: (Bits a, Storable a) => IOVector a -> IO Int
    countNumOnes mvec = count 0 0 (VM.length mvec)
        where
        count :: Int -> Int -> Int -> IO Int
        count !acc i sz
            | i == sz = return acc
            | otherwise = do
                x <- VM.read mvec i
                if testBit x layerNum
                    then count (acc+1) (i+1) sz
                    else count  acc    (i+1) sz

    go i sz out z o
        | i == sz = return ()
        | otherwise = do
            x <- VM.read inputVec i 
            if testBit x layerNum
                then do
                    VM.write out o x
                    go (i+1) sz out z (o+1)
                else do
                    VM.write out z x
                    go (i+1) sz out (z+1) o

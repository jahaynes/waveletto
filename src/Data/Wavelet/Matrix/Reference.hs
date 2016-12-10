
module Data.Wavelet.Matrix.Reference where

import Data.Wavelet.Internal.BitOps                 (rnk1Vector64)
import Data.Wavelet.Matrix                          (WaveletMatrix, getGeometry, getPayload, down, zeroesAtCurrentLayer)
import Data.Wavelet.Matrix.Geometry                 (getInputLength)

import Data.Bits
import Data.Vector.Storable                         (Vector, Storable, (!))
import qualified Data.Vector.Storable   as VS

{- Slow/naive versions of the algorithms.  No need to use these -}

rnk1Slow :: WaveletMatrix -> Int -> Int
rnk1Slow wm i
    | i > (getInputLength . getGeometry $ wm) =
                error "Sanity check failed (i > w64sPerLayer)"
    | otherwise = rnk1Vector64 i (getPayload wm)

waveletMatrixSelectSlow :: Bits a => WaveletMatrix -> a -> Int -> Int
waveletMatrixSelectSlow wm_ a j_ = select' 0 (Just wm_) j_ 0
    where
    select' _   Nothing j p = p + j
    select' l (Just wm) j p
        | testBit a l =
            let p' = zeroesAtCurrentLayer wm + rnk1Slow wm p
                j' = select' (l+1) (down wm) j p'
            in select1VecSlow (getPayload wm) (j'-zeroesAtCurrentLayer wm)
        | otherwise =
            let p' = p - rnk1Slow wm p
                j' = select' (l+1) (down wm) j p'
            in select0VecSlow (getPayload wm) j'

{- Find the position of the nth 1-bit in a vector -}
select0VecSlow :: (FiniteBits a, Storable a) => Vector a -> Int -> Int
select0VecSlow v = go 0 (VS.length v)
    where
    go i len nth
        | i >= len = error "select1Vec rolled off edge"
        | otherwise =
            let x = v ! i
                width = finiteBitSize x
                nth' = nth - (width - popCount x)
            in if nth' < 0
                then (width * i) + select0El x nth
                else go (i+1) len nth'

    {- Find the position of the nth 0-bit in a word -}
    select0El :: FiniteBits a => a -> Int -> Int
    select0El x = go' 0
        where
        go' :: Int -> Int -> Int
        go' i nth | i >= finiteBitSize x = error "select1 rolled off edge"
                | testBit x i = go' (i+1) nth
                | otherwise   = if nth == 0 then i else go' (i+1) (nth-1)

{- Find the position of the nth 1-bit in a vector -}
select1VecSlow :: (FiniteBits a, Storable a) => Vector a -> Int -> Int
select1VecSlow v = go 0 (VS.length v)
    where
    go i len nth
        | i >= len = error "select1Vec rolled off edge"
        | otherwise = 
            let x = v ! i
                nth' = nth - popCount x
            in if nth' < 0
                then (finiteBitSize x * i) + select1El x nth
                else go (i+1) len nth'

    {- Find the position of the nth 1-bit in a word -}
    select1El :: FiniteBits a => a -> Int -> Int
    select1El x = go' 0
        where
        go' :: Int -> Int -> Int
        go' i nth | i >= finiteBitSize x = error "select1 rolled off edge"
                | testBit x i = if nth == 0 then i else go' (i+1) (nth-1)
                | otherwise   = go' (i+1) nth            

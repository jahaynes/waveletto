{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Data.Wavelet.Matrix where

import Data.Wavelet
import Data.Wavelet.Storage
import Data.Wavelet.Auxiliary.RankBlocks       (RankBlocks, createRankBlocks, loadRankBlocks)
import qualified Data.Wavelet.Auxiliary.RankBlocks as R
import Data.Wavelet.Matrix.Geometry                 (Geometry (..))
import qualified Data.Wavelet.Matrix.Geometry as G
import Data.Wavelet.Internal.Buffer
import Data.Wavelet.Internal.BitOps                 (rnk1Vector64)
import Data.Wavelet.Internal.Input as I
import Data.Wavelet.Internal.Partition
import Data.Wavelet.Internal.Shared                 (quot1, removeDirIfExists, removeFileIfExists)
import Data.Wavelet.Internal.Types

import           Control.Monad                      (forM_)
import           Data.Bits                          
import           Data.Vector.Storable               (Vector, Storable, (!))
import qualified Data.Vector.Storable         as VS
import           Data.Vector.Storable.Mutable       (IOVector)
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Vector.Storable.MMap          
import           Data.Word                          (Word64)
import           System.Directory                   (makeAbsolute)

data WaveletMatrix = WaveletMatrix 
                   { getPayload      :: Vector Word64
                   , getZeroCounts   :: ZeroCounts
                   , getGeometry     :: Geometry
                   , getRankBlocks   :: RankBlocks
                   } deriving Show

newtype ZeroCounts = ZeroCounts (Vector Int)
instance Show ZeroCounts where
    show (ZeroCounts vec) = show vec

newtype ZeroBits = ZeroBits Int      

newtype OneBits = OneBits Int

type BitsInLayer = Int

data Buf = Buf {-# UNPACK #-} !Word64
               {-# UNPACK #-} !Int

filenameWaveletMatrix :: FilePath
filenameWaveletMatrix = "wavelet_matrix"

filenameZeroCounts :: FilePath
filenameZeroCounts = "zero_counts"

instance FromDirectory WaveletMatrix where

    {- Load this structure from a directory -}
    load :: IndexPath -> IO (Maybe WaveletMatrix)
    load indexPath = do

        [waveletMatrixPath,zeroCountsPath] <- getFilePaths indexPath

        payload <- unsafeMMapVector waveletMatrixPath Nothing
        zeroCounts <- ZeroCounts <$> unsafeMMapVector zeroCountsPath Nothing
        geometry <- G.loadGeometry indexPath

        rankBlocks <- loadRankBlocks indexPath geometry

        return . Just $ WaveletMatrix
                            payload
                            zeroCounts
                            geometry
                            rankBlocks

    {- Create and return a new structure in this directory from a given vector -}
    create :: (Bits a, Ord a, Storable a) => IndexPath -> Input a -> IO WaveletMatrix
    create indexPath input = do
        removeDirIfExists indexPath
        usingBuffer input indexPath $ createWaveletMatrix input indexPath
        mWaveletMatrix <- load indexPath
        case mWaveletMatrix of
            Nothing -> error "Failed to load newly-created WaveletMatrix"
            Just x -> return x

instance Wavelet WaveletMatrix where

    {- Return the element at a given position -}
    access :: WaveletMatrix -> Position -> a
    access = error "Access not yet defined over Wavelet Matrix"

    {- Count the number of a given symbol up to a given position -}
    rank :: Bits a => WaveletMatrix -> a -> Position -> Count
    rank = waveletMatrixRank

    {- For a given symbol, find its nth occurrence -}
    select :: WaveletMatrix -> a -> Int -> Position
    select = error "Select not yet defined over Wavelet Matrix"

    {- Return the length of the source -}
    length :: WaveletMatrix -> Int
    length = G.getInputLength . getGeometry

waveletMatrixRank :: Bits a => WaveletMatrix -> a -> Int -> Int
waveletMatrixRank wm_ a i_ = rnk' (Just wm_) 0 i_ 0

    where
    rnk' :: Maybe WaveletMatrix -> Int -> Int -> Int -> Int
    rnk'   Nothing _  i p = i - p
    rnk' (Just wm) l  i p
        | testBit a l = let zl = zeroesForLayer wm 0
                            p' = zl + rnk1Fast wm p
                            i' = zl + rnk1Fast wm i
                        in rnk' (down wm) (l+1) i' p'
        | otherwise = let p' = p - rnk1Fast wm p
                          i' = i - rnk1Fast wm i
                      in rnk' (down wm) (l+1) i' p'

    zeroesForLayer :: WaveletMatrix -> Int -> Int
    zeroesForLayer wm l = (\(ZeroCounts zc) -> zc ! l) (getZeroCounts wm)

    rnk1Slow :: WaveletMatrix -> Int -> Int
    rnk1Slow wm i
        | i > (G.getInputLength . getGeometry $ wm) =
                  error "Sanity check failed (i > w64sPerLayer)"
        | otherwise = rnk1Vector64 i (getPayload wm)

    rnk1Fast :: WaveletMatrix -> Int -> Int
    rnk1Fast wm i
        | i > (G.getInputLength . getGeometry $ wm) =
                  error "Sanity check failed (i > w64sPerLayer)"
        | otherwise =
            let (R.RankBlocks _ rbvec) = getRankBlocks wm
                (fullBlocks,remainBits) = i `quotRem` (64 * R.blockSize)
                blockLookup = fullBlocks - 1
                precomputedRank =
                    if blockLookup == -1
                        then 0
                        else rbvec ! blockLookup
                lastBits = rankSkip (fullBlocks * R.blockSize) wm remainBits
            in precomputedRank + lastBits

rankSkip :: Int -> WaveletMatrix -> Int -> Int
rankSkip wordsToSkip wm i = do
    let skippedPayload = VS.drop wordsToSkip
                       . VS.take (getW64sPerLayer . getGeometry $ wm)
                       . getPayload
                       $ wm

    go 0 0 skippedPayload i
    where
    go !acc j vec remaining
        | remaining < 64 =
            let mask = (1 `shiftL` remaining) - 1
            in
            acc + popCount ( mask .&. (vec ! j) )
        | otherwise =
            let count = popCount (vec ! j)
            in
            go (acc+count) (j+1) vec (remaining-64)

down :: WaveletMatrix -> Maybe WaveletMatrix
down wm = case (\(ZeroCounts z) -> VS.drop 1 z) (getZeroCounts wm) of
    z' | VS.null z' -> Nothing
       | otherwise -> Just $ WaveletMatrix
                          (VS.drop (1 * G.getW64sPerLayer (getGeometry wm)) (getPayload wm))
                          (ZeroCounts z')
                          (G.down (getGeometry wm))
                          (R.down (getRankBlocks wm))

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths indexPath =
    mapM makeSubPath [filenameWaveletMatrix,filenameZeroCounts]
    where
    makeSubPath subPath = makeAbsolute . concat $ [indexPath, "/", subPath]
    
createWaveletMatrix :: (Bits a, Ord a, Storable a) => Input a -> IndexPath -> Buffer a ->IO ()
createWaveletMatrix input indexPath buffer = do

    let requiredLayers = getRequiredLayers input
        inputLength = I.getInputLength input
        w64sPerLayer = inputLength `quot1` 64
        outputSize = w64sPerLayer * requiredLayers

    [waveletMatrixPath,zeroCountsPath] <- getFilePaths indexPath

    buffer2Path <- makeAbsolute $ concat [indexPath, "/", "buf2.tmp"]

    payload <- unsafeMMapMVector
                  waveletMatrixPath
                  ReadWriteEx
                  (Just (0, outputSize)) :: IO (IOVector Word64)

    zeroCounts <- unsafeMMapMVector
                      zeroCountsPath
                      ReadWriteEx
                      (Just (0, requiredLayers)) :: IO (IOVector Int)

    let geometry = Geometry inputLength requiredLayers w64sPerLayer
    G.saveGeometry indexPath geometry

    forM_ (getLayerRange requiredLayers) $ \layer -> do
        vs <- VS.unsafeFreeze (getBuffer buffer)
        zeroes <- gatherAndWriteOutBits layer vs payload
        setZeroCount zeroCounts layer zeroes      
        inPlaceStableSortByBitN (getBuffer buffer) buffer2Path layer

    removeFileIfExists buffer2Path

    payload' <- VS.unsafeFreeze payload
    _ <- createRankBlocks indexPath payload' geometry
    return ()

    where
    setZeroCount :: IOVector Int -> LayerNum -> ZeroBits -> IO ()
    setZeroCount vm (LayerNum l) (ZeroBits z) = VM.write vm l z

    getLayerRange :: RequiredLayers -> [LayerNum]
    getLayerRange requiredLayers = [LayerNum 0..LayerNum (requiredLayers-1)]

    gatherAndWriteOutBits :: (Bits a, Storable a) => LayerNum -> Vector a -> IOVector Word64 -> IO ZeroBits
    gatherAndWriteOutBits (LayerNum l) vs dest = do

        let bitsInLayer = I.getInputLength input
            sizeOfLayer = bitsInLayer `quot1` 64
            writeOffset = l * sizeOfLayer
        OneBits ones <- writeOutBitLayer writeOffset gatherBitLayer
        return $ ZeroBits (bitsInLayer - ones)

        where
        gatherBitLayer :: [Word64]
        gatherBitLayer = go 0 (VS.length vs) (Buf 0 0)
            where
            go :: Int -> Int -> Buf -> [Word64]
            go i j (Buf buf fillSz)
                | i == j             = [buf]
                | fillSz == 64       =  buf : go  i    j (Buf           0               0    )
                | testBit (vs ! i) l =        go (i+1) j (Buf (setBit buf fillSz) (fillSz+1) )
                | otherwise          =        go (i+1) j (Buf         buf         (fillSz+1) )

        writeOutBitLayer :: Int -> [Word64] -> IO OneBits
        writeOutBitLayer = go 0
            where
            go :: Int -> Int -> [Word64] -> IO OneBits
            go !acc _     [] = return (OneBits acc)
            go  acc i (x:xs) = do
                VM.write dest i x
                go (acc + popCount x) (i+1) xs

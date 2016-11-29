{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Data.Wavelet.Matrix where

import Data.Wavelet
import Data.Wavelet.Storage
import Data.Wavelet.Auxiliary.RankBlocks            (RankBlocks)
import Data.Wavelet.Internal.Buffer
import Data.Wavelet.Internal.BitOps                 (rnk1Vector64)
import Data.Wavelet.Internal.Input
import Data.Wavelet.Internal.Partition
import Data.Wavelet.Internal.Shared                 (quot1, removeDirIfExists, removeFileIfExists)
import Data.Wavelet.Internal.Types

import           Control.Monad                      (forM_)
import           Data.Bits                          (Bits, popCount, setBit, testBit)
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
                   , getSignalLength :: {-# UNPACK #-} !Int
                   , getNumLayers    :: {-# UNPACK #-} !RequiredLayers
                   , getW64sPerLayer :: {-# UNPACK #-} !Int
                   , getRankBlocks   :: Maybe RankBlocks
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

filenameGeometry :: FilePath
filenameGeometry = "geometry"

instance FromDirectory WaveletMatrix where

    {- Load this structure from a directory -}
    load :: IndexPath -> IO (Maybe WaveletMatrix)
    load indexPath = do

        [waveletMatrixPath,zeroCountsPath,geometryPath] <- getFilePaths indexPath

        payload <- unsafeMMapVector waveletMatrixPath Nothing
        zeroCounts <- ZeroCounts <$> unsafeMMapVector zeroCountsPath Nothing
        [inputLength, requiredLayers, w64sPerLayer] <-
            VS.toList . VS.take 3 <$> unsafeMMapVector geometryPath Nothing

        let maybeRankBlocks = undefined

        return . Just $ WaveletMatrix
                            payload
                            zeroCounts
                            inputLength
                            requiredLayers
                            w64sPerLayer
                            maybeRankBlocks

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
    length = error "Length not yet defined over Wavelet Matrix"

waveletMatrixRank :: Bits a => WaveletMatrix -> a -> Int -> Int
waveletMatrixRank wm_ a i_ = rnk' (Just wm_) 0 i_ 0

    where
    rnk' :: Maybe WaveletMatrix -> Int -> Int -> Int -> Int
    rnk'   Nothing _  i p = i - p
    rnk' (Just wm) l  i p
        | testBit a l = let zl = zeroesForLayer wm 0
                            p' = zl + rnk1 wm p
                            i' = zl + rnk1 wm i
                        in rnk' (down wm 1) (l+1) i' p'
        | otherwise = let p' = p - rnk1 wm p
                          i' = i - rnk1 wm i
                      in rnk' (down wm 1) (l+1) i' p'

    zeroesForLayer :: WaveletMatrix -> Int -> Int
    zeroesForLayer wm l = (\(ZeroCounts zc) -> zc ! l) (getZeroCounts wm)

    rnk1 :: WaveletMatrix -> Int -> Int
    rnk1 (WaveletMatrix payload _ signalLength _ _ _) i
        | i > signalLength = error "Sanity check failed (i > w64sPerLayer)"
        | otherwise = rnk1Vector64 i payload

down :: WaveletMatrix -> Int -> Maybe WaveletMatrix
down wm n = case (\(ZeroCounts z) -> VS.drop n z) (getZeroCounts wm) of
    z' | VS.null z' -> Nothing
       | otherwise -> Just $ WaveletMatrix
                          (VS.drop (n * getW64sPerLayer wm) (getPayload wm))
                          (ZeroCounts z')
                          (getSignalLength wm)
                          (getNumLayers wm - n)
                          (getW64sPerLayer wm)
                          (getRankBlocks wm)

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths indexPath =
    mapM (\subPath -> makeAbsolute . concat $ [indexPath, "/", subPath])
        [filenameWaveletMatrix,filenameZeroCounts,filenameGeometry]                              

augment :: WaveletMatrix -> IO WaveletMatrix
augment wm = undefined

createWaveletMatrix :: (Bits a, Ord a, Storable a) => Input a -> IndexPath -> Buffer a ->IO ()
createWaveletMatrix input indexPath buffer = do

    let requiredLayers = getRequiredLayers input
        inputLength = getInputLength input
        w64sPerLayer = inputLength `quot1` 64
        outputSize = w64sPerLayer * requiredLayers

    [waveletMatrixPath,zeroCountsPath,geometryPath] <- getFilePaths indexPath

    buffer2Path <- makeAbsolute $ concat [indexPath, "/", "buf2.tmp"]

    matrix <- unsafeMMapMVector
                  waveletMatrixPath
                  ReadWriteEx
                  (Just (0, outputSize)) :: IO (IOVector Word64)

    zeroCounts <- unsafeMMapMVector
                      zeroCountsPath
                      ReadWriteEx
                      (Just (0, requiredLayers)) :: IO (IOVector Int)

    geometryFile <- unsafeMMapMVector
                       geometryPath
                       ReadWriteEx
                       (Just (0, 3))

    VS.copy geometryFile (VS.fromList [inputLength, requiredLayers, w64sPerLayer])
    
    forM_ (getLayerRange requiredLayers) $ \layer -> do
        vs <- VS.unsafeFreeze (getBuffer buffer)
        zeroes <- gatherAndWriteOutBits layer vs matrix
        setZeroCount zeroCounts layer zeroes      
        inPlaceStableSortByBitN (getBuffer buffer) buffer2Path layer
        
    removeFileIfExists buffer2Path

    where
    setZeroCount :: IOVector Int -> LayerNum -> ZeroBits -> IO ()
    setZeroCount vm (LayerNum l) (ZeroBits z) = VM.write vm l z

    getLayerRange :: RequiredLayers -> [LayerNum]
    getLayerRange requiredLayers = [LayerNum 0..LayerNum (requiredLayers-1)]

    gatherAndWriteOutBits :: (Bits a, Storable a) => LayerNum -> Vector a -> IOVector Word64 -> IO ZeroBits
    gatherAndWriteOutBits (LayerNum l) vs dest = do

        let bitsInLayer = getInputLength input
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

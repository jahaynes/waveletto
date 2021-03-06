{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Data.Wavelet.Matrix where

import           Data.Wavelet
import           Data.Wavelet.Bulk
import           Data.Wavelet.Storage
import           Data.Wavelet.Auxiliary.RankBlocks            (RankBlocks (..), createRankBlocks, loadRankBlocks)
import qualified Data.Wavelet.Auxiliary.RankBlocks   as R
import           Data.Wavelet.Auxiliary.SelectBlocks as S
import           Data.Wavelet.Matrix.Geometry                 (Geometry (..), loadGeometry, saveGeometry, getW64sPerLayer)
import qualified Data.Wavelet.Matrix.Geometry        as G     (down, getInputLength)
import           Data.Wavelet.Internal.Buffer                 (Buffer, usingBuffer, getBuffer)
import           Data.Wavelet.Internal.Input as I             (Input, getRequiredLayers, getInputLength)         
import           Data.Wavelet.Internal.Partition              (inPlaceStableSortByBitN)
import           Data.Wavelet.Internal.Shared                 (quot1, removeDirIfExists, removeFileIfExists)
import           Data.Wavelet.Internal.Types

import           Control.Monad                      (forM_)
import           Data.Bits                          
import           Data.Vector.Storable               (Vector, Storable, (!))
import qualified Data.Vector.Storable         as VS
import           Data.Vector.Storable.Mutable       (IOVector)
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Vector.Storable.MMap          
import           Data.Word                          
import           System.Directory                   (makeAbsolute)

data WaveletMatrix = WaveletMatrix
                   { getPayload       :: Vector Word64
                   , getZeroCounts    :: ZeroCounts
                   , getGeometry      :: Geometry
                   , getRankBlocks    :: RankBlocks
                   , getSelect0Blocks :: Select0Blocks
                   , getSelect1Blocks :: Select1Blocks
                   }

newtype ZeroCounts = ZeroCounts (Vector Int)

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
    load :: IndexPath -> IO WaveletMatrix
    load indexPath = do

        [waveletMatrixPath,zeroCountsPath] <- getFilePaths indexPath

        payload <- unsafeMMapVector waveletMatrixPath Nothing
        zeroCounts <- ZeroCounts <$> unsafeMMapVector zeroCountsPath Nothing
        geometry <- loadGeometry indexPath

        rankBlocks <- loadRankBlocks indexPath geometry
        (se0, se1) <- loadSelectBlocks indexPath geometry

        return $ WaveletMatrix
                     payload
                     zeroCounts
                     geometry
                     rankBlocks
                     se0
                     se1

    {- Create and return a new structure in this directory from a given vector -}
    create :: (Bits a, Ord a, Storable a) => IndexPath -> Input a -> IO WaveletMatrix
    create indexPath input = do
        removeDirIfExists indexPath
        usingBuffer input indexPath $
            createWaveletMatrix input indexPath

instance Wavelet WaveletMatrix where

    {- Return the element at a given position -}
    access :: Bits a => WaveletMatrix -> Position -> a
    access = waveletMatrixAccess
    {-# INLINABLE access #-}
    {-# SPECIALIZE access :: WaveletMatrix -> Position -> Word8  #-}
    {-# SPECIALIZE access :: WaveletMatrix -> Position -> Word32 #-}

    {- Count the number of a given symbol up to a given position -}
    rank :: Bits a => WaveletMatrix -> a -> Position -> Count
    rank = waveletMatrixRank
    {-# INLINABLE rank #-}
    {-# SPECIALIZE rank :: WaveletMatrix -> Word8  -> Position -> Count #-}
    {-# SPECIALIZE rank :: WaveletMatrix -> Word32 -> Position -> Count #-}

    {- For a given symbol, find its nth occurrence -}
    select :: Bits a => WaveletMatrix -> a -> Int -> Position
    select = waveletMatrixSelect
    {-# INLINABLE select #-}
    {-# SPECIALIZE select :: WaveletMatrix -> Word8  -> Int -> Position #-}
    {-# SPECIALIZE select :: WaveletMatrix -> Word32 -> Int -> Position #-}

    {- Return the length of the source -}
    getInputLength :: WaveletMatrix -> Int
    getInputLength = G.getInputLength . getGeometry

instance BulkWavelet WaveletMatrix where

    {- Return many elements when given many positions -}
    accessMany :: (Bits a, Storable a) => WaveletMatrix -> Vector Position -> Vector a
    accessMany = waveletMatrixAccessMany
    {-# INLINABLE accessMany #-}
    {-# SPECIALIZE accessMany :: WaveletMatrix -> Vector Position -> Vector Word8  #-}
    {-# SPECIALIZE accessMany :: WaveletMatrix -> Vector Position -> Vector Word32 #-}

waveletMatrixAccessMany :: (Bits a, Storable a) => WaveletMatrix -> Vector Position -> Vector a
waveletMatrixAccessMany wm_ ps_ = accs' (VS.replicate (VS.length ps_) zeroBits) 0 (Just wm_) ps_
    where
    accs' !vAcc _   Nothing  _ = vAcc
    accs'  vAcc l (Just wm) ps = do

        let testeds = VS.map (\p -> do
                -- Find which bits in which words to test
                let (wd, bt) = p `quotRem` 64
                --Test them
                testBit (getPayload wm ! wd) bt) ps

        -- Work out the positions to test in the next layer
            ps' = VS.zipWith (\t p -> if t
                                          then rnk1Fast wm p + zeroesAtCurrentLayer wm
                                          else p - rnk1Fast wm p) testeds ps

        -- Set bits on our accumulators
            vAcc' = VS.zipWith (\t a -> if t
                                          then setBit a l
                                          else a) testeds vAcc

        -- Process the next layer
        accs' vAcc' (l+1) (down wm) ps'

waveletMatrixAccess :: Bits a => WaveletMatrix -> Position -> a
waveletMatrixAccess wm_ = accs' zeroBits 0 (Just wm_)
    where
    accs' !acc _   Nothing _ = acc
    accs'  acc l (Just wm) i = do

        -- Find which bit in which word to test
        let (wd, bt) = i `quotRem` 64

        -- Test it
            tested = testBit (getPayload wm ! wd) bt

        -- Work out the position to test in the next layer
            i' = if tested
                     then rnk1Fast wm i + zeroesAtCurrentLayer wm
                     else i - rnk1Fast wm i

        -- If we processed a set bit, set a bit on our accumulator
            acc' = if tested
                     then setBit acc l
                     else acc

        -- Process the next layer
        accs' acc' (l+1) (down wm) i'

waveletMatrixRank :: Bits a => WaveletMatrix -> a -> Position -> Count
waveletMatrixRank wm_ a i_ = rnk' (Just wm_) 0 i_ 0

    where
    rnk' :: Maybe WaveletMatrix -> Int -> Int -> Int -> Count
    rnk'   Nothing _  i p = i - p
    rnk' (Just wm) l  i p
        | testBit a l = let zl = zeroesAtCurrentLayer wm
                            p' = zl + rnk1Fast wm p
                            i' = zl + rnk1Fast wm i
                        in rnk' (down wm) (l+1) i' p'
        | otherwise = let p' = p - rnk1Fast wm p
                          i' = i - rnk1Fast wm i
                      in rnk' (down wm) (l+1) i' p'

waveletMatrixSelect :: Bits a => WaveletMatrix -> a -> Int -> Int
waveletMatrixSelect wm_ a j_ = select' 0 (Just wm_) j_ 0
    where
    select' _   Nothing j p = p + j
    select' l (Just wm) j p
        | testBit a l =
            let p' = zeroesAtCurrentLayer wm + rnk1Fast wm p
                j' = select' (l+1) (down wm) j p'
            in S.select1WithLookup (getPayload wm) (getSelect1Blocks wm) (j'-zeroesAtCurrentLayer wm)
        | otherwise =
            let p' = p - rnk1Fast wm p
                j' = select' (l+1) (down wm) j p'
            in S.select0WithLookup (getPayload wm) (getSelect0Blocks wm) j'

zeroesAtCurrentLayer :: WaveletMatrix -> Int
zeroesAtCurrentLayer = (\(ZeroCounts zc) -> zc ! 0) . getZeroCounts

rnk1Fast :: WaveletMatrix -> Int -> Int
rnk1Fast wm i
    | i > (G.getInputLength . getGeometry $ wm) =
                error "Sanity check failed (i > w64sPerLayer)"
    | otherwise =
        let (RankBlocks _ rbvec) = getRankBlocks wm
            (fullBlocks,remainBits) = i `quotRem` (64 * R.blockSize)
            blockLookup = fullBlocks - 1
            precomputedRank =
                if blockLookup == -1
                    then 0
                    else rbvec ! blockLookup
            lastBits = rankSkip (fullBlocks * R.blockSize) remainBits
        in precomputedRank + lastBits

    where
    rankSkip :: Int -> Int -> Int
    rankSkip wordsToSkip = go 0 wordsToSkip (getPayload wm)
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
                          (VS.drop (1 * getW64sPerLayer (getGeometry wm)) (getPayload wm))
                          (ZeroCounts z')
                          (G.down (getGeometry wm))
                          (R.down (getRankBlocks wm))
                          (S.down0 (getSelect0Blocks wm))
                          (S.down1 (getSelect1Blocks wm))

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths indexPath =
    mapM makeSubPath [filenameWaveletMatrix,filenameZeroCounts]
    where
    makeSubPath subPath = makeAbsolute . concat $ [indexPath, "/", subPath]

createWaveletMatrix :: (Bits a, Ord a, Storable a) => Input a -> IndexPath -> Buffer a -> IO WaveletMatrix
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
    saveGeometry indexPath geometry

    forM_ (getLayerRange requiredLayers) $ \layer -> do
        vs <- VS.unsafeFreeze (getBuffer buffer)
        zeroes <- gatherAndWriteOutBits layer vs payload
        setZeroCount zeroCounts layer zeroes      
        inPlaceStableSortByBitN (getBuffer buffer) buffer2Path layer

    removeFileIfExists buffer2Path

    payload' <- VS.unsafeFreeze payload
    zeroCounts' <- ZeroCounts <$> VS.unsafeFreeze zeroCounts
    rankBlocks <- createRankBlocks indexPath payload' geometry
    (se0, se1) <- createSelectBlocks indexPath payload' geometry

    return $ WaveletMatrix
                 payload'
                 zeroCounts'
                 geometry
                 rankBlocks
                 se0
                 se1

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

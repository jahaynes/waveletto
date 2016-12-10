module Data.Wavelet.Matrix.Geometry where

import Data.Wavelet.Internal.Types

import Data.Vector.Storable         (copy, fromList, toList)
import Data.Vector.Storable.MMap    
import System.Directory             (makeAbsolute)

data Geometry = Geometry
              { getInputLength  :: {-# UNPACK #-} !Int
              , getNumLayers    :: {-# UNPACK #-} !Int
              , getW64sPerLayer :: {-# UNPACK #-} !Int
              }

loadGeometry :: IndexPath -> IO Geometry
loadGeometry indexPath = do
    path <- getGeometryPath indexPath
    [il,nl,wpl] <- toList <$> unsafeMMapVector path Nothing
    return $ Geometry il nl wpl

saveGeometry :: IndexPath -> Geometry -> IO ()
saveGeometry indexPath (Geometry il nl wpl) = do
    path <- getGeometryPath indexPath
    out <- unsafeMMapMVector path ReadWriteEx (Just (0,3))
    copy out (fromList [il, nl, wpl])

getGeometryPath :: IndexPath -> IO FilePath
getGeometryPath indexPath = makeAbsolute (indexPath ++ "/geometry")

-- Step "down" one layer
down :: Geometry -> Geometry
down (Geometry inputLength numLayers w64sPerLayer) =
    Geometry inputLength (numLayers - 1) w64sPerLayer

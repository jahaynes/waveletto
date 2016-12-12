# waveletto
Wavelet Matrices

    import Data.Wavelet.Storage         (create, load)
    import Data.Wavelet.Internal.Input  (prepareInput)
    import Data.Wavelet.Matrix          (WaveletMatrix)
    import Data.Wavelet                 (access, rank, select)

    import qualified Data.Vector.Storable as VS

    main :: IO ()
    main = do
        
        --Define some input
        let input = VS.fromList [1, 2, 3, 4, 1, 2, 3, 4,
                                 5, 6, 7, 8, 5, 6, 7, 8,
                                 9,10,11,12, 9,10,11,12] :: VS.Vector Int

            len = VS.length input

        --Create a wavelet matrix on disk at "somepath"
        matrix <- create "somePath" (prepareInput input) :: IO WaveletMatrix

        --Load an existing wavelet matrix at "somepath"
        --matrix <- load "somePath" :: IO WaveletMatrix

        -- Retrieve the element at position 7
        print (access matrix 7 :: Int)

        -- Count how many instances of '3' there are in the range [0,len)
        print (rank matrix (3::Int) len)

        -- Find the second occurrence of '5'
        print (select matrix (5::Int) 1)

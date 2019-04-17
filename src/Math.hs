module Math(hamming, preemphasis, sliding, toMesh, spectrogram) where

import RIO
import RIO.List (mapAccumL, zipWith)
import Control.Parallel.Strategies
import Data.Complex
import qualified Numeric.FFT as NF

sliding :: Int -> Int -> a -> [a] -> [[a]]
sliding step size padding xs
    | len < size = [xs ++ replicate (size - len) padding]
    | otherwise  = take size xs : sliding step size padding (drop step xs)
    where len = length xs

preemphasis :: Complex Double -> [Complex Double] -> [Complex Double]
preemphasis emph = snd . mapAccumL (\prev y -> (y, y - emph * prev)) 0.0

hamming :: [Complex Double] -> [Complex Double]
hamming signal = let len = length signal
                  in zipWith (*) signal $ hamming' len 0.53

hamming' :: Int -> Complex Double -> [Complex Double]
hamming' n alpha =
    let beta = 1 - alpha
     in [alpha - beta * cos(2 * pi * fromIntegral i / (fromIntegral n - 1)) | i <- [0..n - 1]]

toMesh :: Num c => [[c]] -> [[(c, c, c)]]
toMesh dd = do
    (x, fs) <- zip [1..] dd
    return $ do
        (y, p) <- zip [0..] fs
        return (fromIntegral x, fromIntegral y, p)

spectrogram :: Int -> Int -> [Complex Double] -> [[Complex Double]]
spectrogram step len signal = let ft = take (len `div` 2) . NF.fft . hamming
                                  windows = sliding step len 0 signal
                                  nWindows = length windows
                                  strat = parListChunk (nWindows `div` 4) rdeepseq
                              in ft <$> windows `using` strat

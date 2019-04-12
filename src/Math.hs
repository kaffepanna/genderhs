{-# LANGUAGE FlexibleContexts #-}
module Math(hamming, preemphasis, sliding, dft, toMesh) where
    
import Data.List (mapAccumL)
import Data.Complex

sliding :: Int -> Int -> a -> [a] -> [[a]]
sliding step size padding xs
    | len < size = [xs ++ replicate (size - len) padding]
    | otherwise  = take size xs : sliding step size padding (drop step xs)
    where len = length xs

preemphasis :: Complex Double -> [Complex Double] -> [Complex Double]
preemphasis emph = snd . mapAccumL (\prev y -> (y, y - emph * prev)) 0.0

hamming :: [Complex Double] -> [Complex Double]
hamming signal = let len = fromIntegral $ length signal
                  in zipWith (*) signal $ hamming' len 0.53

hamming' :: Int -> Complex Double -> [Complex Double]
hamming' length alpha = 
    let beta = 1 - alpha
     in [alpha - beta * cos(2 * pi * fromIntegral i / (fromIntegral length - 1)) | i <- [0..length - 1]] 
    
dft :: (Num a, RealFloat a) => [Complex a] -> [Complex a]
dft signal = inner <$> [0..nn-1]
    where i = 0 :+ 1
          cindex = fromIntegral <$> [0..] :: (RealFloat a) => [Complex a]
          nn = fromIntegral $ length signal
          inner k = sum [ xn * exp ( -i * 2 * pi / fromIntegral nn * fromIntegral k * n ) | (n, xn) <- zip cindex signal]

toMesh :: (Num c) => [[c]] -> [[(c, c, c)]]
toMesh dd = do
    (x, fs) <- zip [0..] dd
    return $ do
        (y, p) <- zip [0..] fs
        return (fromIntegral x, fromIntegral y, p)



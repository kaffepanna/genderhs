{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Main where

import Math
import Data.Int (Int16)
import Data.List (transpose)
import Data.Complex
import Data.Binary
import Data.Binary.Get
import Graphics.Gnuplot.Simple
import Codec.Audio.Wave
import Conduit ((.|))
import qualified Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Serialization.Binary as CSB
import qualified Data.ByteString as BS
import qualified Numeric.FFT as NF

 
plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]


plot :: [[Double]] -> IO ()
plot power = do
    putStrLn $ "XX: " ++ show xx
    putStrLn $ "YY: " ++ show yy
    plotFunc3d [] plotAttrs [0..xx-1] [0..yy-1] (\x y -> log $ (power !! x) !! y)
    return ()
        where xx = length power
              yy = length $ head power

filename :: FilePath
filename = "data/1028-20100710-hne/wav/ar-04.wav"
--filename = "/home/patrik/Downloads/100Hz_44100Hz_16bit_05sec.wav"

decoder :: (C.MonadThrow m) => C.ConduitT BS.ByteString Int16 m ()
decoder = CSB.conduitGet getInt16le

main :: IO ()
main = do
    wf <- readWaveFile filename 
    let dataOffset = fromIntegral $ waveDataOffset wf
        sampleRate = fromIntegral $ waveSampleRate wf
        samplesTotal = fromIntegral $ waveSamplesTotal wf
        bitsPerSample = waveBitsPerSample wf
        dataSize = fromIntegral $ waveDataSize wf
        blockAlign = waveBlockAlign wf
        fileFormat = waveFileFormat wf


    putStrLn $ "sample rate: " ++ show sampleRate
    putStrLn $ "n samples: " ++ show samplesTotal
    putStrLn $ "file format: " ++ show fileFormat

    samples <- C.runResourceT . C.runConduit $
        CB.sourceFileRange filename (Just dataOffset) (Just dataSize)
            .| decoder
            .| C.sinkList

    let signal :: [Complex Double]
        signal = preemphasis 0.96 $ fromIntegral <$> samples
        spectrum = take 256 . NF.fft . hamming <$> sliding 256 512 0 signal
        power = (map . map) magnitude spectrum :: [[Double]]
        time =  (/ sampleRate) <$> [0..samplesTotal-1] :: [Double]
        powerTime = (map . map) (\(x,y, z) -> (256*x/sampleRate, y*sampleRate/512, log z**2)) $ toMesh power 

    --plotList [] $ zip time samples
    --`plotList [] $ zip time $ realPart <$> signal
    --plot power
    plotMesh3d [] plotAttrs powerTime
    getLine
    return ()




module Main where

import RIO
import qualified RIO.ByteString as BS
import Data.Int (Int16)
import Data.Binary.Get
import Graphics.Gnuplot.Simple
import qualified Conduit as C
import qualified Data.Conduit.Serialization.Binary as CSB
import SpeechCorpus (dmain)

plotAttrs :: [Attribute3d]
plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]

filename :: FilePath
filename = "data/1028-20100710-hne/wav/ar-04.wav"
--filename = "/home/patrik/Downloads/100Hz_44100Hz_16bit_05sec.wav"

decoder :: (C.MonadThrow m) => C.ConduitT BS.ByteString Int16 m ()
decoder = CSB.conduitGet getInt16le

main :: IO ()
main = runSimpleApp $ do 
    SpeechCorpus.dmain
    return ()

-- main :: IO ()
-- main = do
--     wf <- readWaveFile filename 
--     let dataOffset = fromIntegral $ waveDataOffset wf
--         sampleRate = fromIntegral $ waveSampleRate wf
--         samplesTotal = fromIntegral $ waveSamplesTotal wf
--         bitsPerSample = waveBitsPerSample wf
--         dataSize = fromIntegral $ waveDataSize wf
--         blockAlign = waveBlockAlign wf
--         fileFormat = waveFileFormat wf
-- 
-- 
--     putStrLn $ "sample rate: " ++ show sampleRate
--     putStrLn $ "n samples: " ++ show samplesTotal
--     putStrLn $ "file format: " ++ show fileFormat
-- 
--     samples <- C.runResourceT . C.runConduit $
--         CB.sourceFileRange filename (Just dataOffset) (Just dataSize)
--             .| decoder
--             .| C.sinkList
-- 
--     samples `deepseq` return ()
-- 
--     let signal :: [Complex Double]
--         signal = preemphasis 0.96 $ fromIntegral <$> samples
--         spectrum = spectrogram 256 512 signal
--         power = (map . map) magnitude spectrum :: [[Double]]
--         time =  (/ sampleRate) <$> [0..samplesTotal-1] :: [Double]
--         powerTime = (map . map) (\(x,y, z) -> (256*x/sampleRate, y*sampleRate/512, log z**2)) $ toMesh power 
-- 
--     --plotList [] $ zip time samples
--     --`plotList [] $ zip time $ realPart <$> signal
--     -- plot power
--     plotMesh3d [] plotAttrs powerTime
--     --
--     spectrum `deepseq` return ()
-- 
--     getLine
--     return ()




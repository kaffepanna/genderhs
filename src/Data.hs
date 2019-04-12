{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Data where

import qualified Data.Text as T
import qualified Data.Conduit.Text as CT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CH8
import Conduit
import Network.HTTP.Conduit (responseBody, parseRequest)
import Network.HTTP.Simple (httpSource)
import Text.HTML.DOM (sinkDoc)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Tar (filePath, restoreFileIntoLenient, untarWithExceptions)
import Text.XML.Cursor ((>=>), ($//), (&/), (&|), Cursor, element, attribute, check, fromDocument)
import System.Directory
import System.FilePath (takeBaseName, joinPath)

baseUrl :: String
baseUrl = "http://www.repository.voxforge1.org/downloads/SpeechCorpus/Trunk/Audio/Main/16kHz_16bit/"


notCached :: (MonadIO m) => FilePath -> m Bool
notCached f = not <$> isCached f

isCached :: (MonadIO m) => FilePath -> m Bool
isCached f = do
    let path = joinPath ["./data", takeBaseName f]
    liftIO $ doesDirectoryExist path

sourceUrls :: Monad m => Cursor -> ConduitT i String m ()
sourceUrls c = mapM_ yield $ c
                           $// element "pre"
                           &/ element "a" >=> check (T.isSuffixOf ".tgz" . T.concat . attribute "href")
                           &| T.unpack . T.concat . attribute "href"

download :: (PrimMonad m, MonadResource m, MonadThrow m) => String -> ConduitT String FilePath m () 
download path = do
    liftIO $ putStr "Downloading... "
    req <- liftIO $ parseRequest (baseUrl ++ path)
    f <- httpSource req responseBody .| ungzip .| untarWithExceptions (restoreFileIntoLenient "data")
    yield $ takeBaseName path

decodeFile f = sourceFile f .| CT.decode CT.utf8 .| filterC (T.isInfixOf "Female") .| mapC (const f)



branchIf :: (Monad m) => (i -> m Bool) -> ConduitT i o m () -> ConduitT i o m () -> ConduitT i o m ()
branchIf f c1 c2 = awaitForever $ \elem -> do
    b <- lift $ f elem
    if b then yield elem .| c2
         else yield elem .| c1


-- main :: IO ()
-- main = runResourceT . runConduit $ do
--     request <- parseRequest baseUrl
--     doc <- httpSource request responseBody .| sinkDoc
--     let cursor = fromDocument doc
--     sourceUrls cursor .| takeC 4 .| branchIf notCached ( mapC takeBaseName     )
--                                                        ( awaitForever download ) .| mapM_C (liftIO . print)
-- 

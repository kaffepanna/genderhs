module SpeechCorpus where
import RIO
import qualified RIO.Text as T
import RIO.Directory
import RIO.FilePath (takeBaseName, joinPath)
import Network.HTTP.Simple (httpSource)
import Text.HTML.DOM (sinkDoc)
import Control.Monad.Trans.Resource (runResourceT)
import Conduit
import qualified Data.Conduit.Text as CT
import Network.HTTP.Conduit (responseBody, parseRequest)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Tar (restoreFileIntoLenient, untarWithExceptions)
import Text.XML.Cursor ((>=>), ($//), (&/), (&|), Cursor, element, attribute, check, fromDocument)

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
download filename = do
    req <- liftIO $ parseRequest (baseUrl ++ filename)
    _ <- httpSource req responseBody .| ungzip .| untarWithExceptions (restoreFileIntoLenient "data")
    yield $ takeBaseName filename

decodeFile :: (MonadResource m, MonadThrow m) => FilePath -> ConduitM a FilePath m ()
decodeFile f = sourceFile f .| CT.decode CT.utf8 .| filterC (T.isInfixOf "Female") .| mapC (const f)

branchIf :: (Monad m) => (i -> m Bool) -> ConduitT i o m () -> ConduitT i o m () -> ConduitT i o m ()
branchIf f c1 c2 = awaitForever $ \e -> do
    b <- lift $ f e
    if b then yield e .| c2
         else yield e .| c1

downloadPipe :: (MonadIO m, PrimMonad m, MonadResource m, MonadThrow m) => ConduitT FilePath String m ()
downloadPipe = branchIf notCached ( mapC takeBaseName     )
                                  ( awaitForever download )

dmain :: (PrimMonad m, MonadThrow m, MonadReader env m, MonadUnliftIO m, HasLogFunc env) => m ()
dmain = runResourceT . runConduit $ do
    request <- parseRequest baseUrl
    doc <- httpSource request responseBody .| sinkDoc
    let cursor = fromDocument doc

    sourceUrls cursor .| takeC 40 .| downloadPipe .| mapM_C (logInfo . displayShow)

    logInfo "Hello world"
    return ()

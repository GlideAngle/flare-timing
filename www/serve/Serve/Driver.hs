module Serve.Driver (driverRun) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant
    ( (:<|>)(..)
    , Get, JSON, Server, Handler(..), Proxy(..), ServantErr
    , (:>)
    , err400, errBody, hoistServer, serve, throwError
    )
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, MonadReader, ask, liftIO, runReaderT)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, lift)
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.Directory (doesFileExist)
import System.FilePath (FilePath)
import Data.Yaml (prettyPrintParseException, decodeEither')
import qualified Data.ByteString as BS

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Flight.Comp
    ( CompSettings(..)
    , Comp
    , Task
    , Nominal
    , PilotTrackLogFile(..)
    , Pilot(..)
    )

type FlareTimingApi = CompApi :<|> TaskApi

type CompApi =
    "comps" :> Get '[JSON] [Comp]
    :<|> "nominals" :> Get '[JSON] [Nominal]

type TaskApi =
    "tasks" :> Get '[JSON] [Task Double]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

newtype Config = Config { path :: FilePath }

newtype AppT m a =
    AppT
        { unApp :: ReaderT Config (ExceptT ServantErr m) a
        }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadError ServantErr
        , MonadIO
        )

compApi :: Proxy CompApi
compApi = Proxy

taskApi :: Proxy TaskApi
taskApi = Proxy

flareTimingApi :: Proxy FlareTimingApi
flareTimingApi = Proxy

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt =
    Handler $ runReaderT (unApp appt) cfg

driverRun :: IO ()
driverRun = withCmdArgs drive

drive :: ServeOptions -> IO ()
drive ServeOptions{..} = do
    dfe <- doesFileExist file
    if dfe
        then go file
        else putStrLn "Couldn't find the flight score competition yaml input file."
    where
        port = 3000

        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings

        go path =
            runSettings settings =<< mkApp (Config path)

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: Config -> IO Application
mkApp cfg = do
    let sc = serverComp cfg
    let st = serverTask cfg
    return . simpleCors . serve flareTimingApi $ sc :<|> st

serverComp :: Config -> Server CompApi
serverComp cfg =
    hoistServer compApi (convertApp cfg) (queryComps :<|> queryNominals)

serverTask :: Config -> Server TaskApi
serverTask cfg =
    hoistServer taskApi (convertApp cfg) (queryTasks :<|> queryPilots)

queryWith :: (FilePath -> IO (Either String a)) -> AppT IO a
queryWith f = do
    path' <- path <$> ask
    xs <- liftIO $ f path'
    case xs of
      Left msg -> throwError $ err400 { errBody = LBS.pack msg }
      Right xs' -> return xs'

yamlComps :: FilePath -> ExceptT String IO [Comp]
yamlComps yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither' contents of
        Left msg -> throwE . prettyPrintParseException $ msg
        Right CompSettings{..} -> ExceptT . return $ Right [comp]

yamlNominals :: FilePath -> ExceptT String IO [Nominal]
yamlNominals yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither' contents of
        Left msg -> throwE . prettyPrintParseException $ msg
        Right CompSettings{..} -> ExceptT . return $ Right [nominal]

yamlTasks :: FilePath -> ExceptT String IO [Task Double]
yamlTasks yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither' contents of
        Left msg -> throwE . prettyPrintParseException $ msg
        Right CompSettings{..} -> ExceptT . return $ Right tasks

yamlPilots :: FilePath -> ExceptT String IO [[Pilot]]
yamlPilots yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither' contents of
        Left msg -> throwE . prettyPrintParseException $ msg
        Right CompSettings{..} -> ExceptT . return $ Right $ (fmap . fmap) pilot pilots
    where
        pilot (PilotTrackLogFile p _) = p

queryComps :: AppT IO [Comp]
queryComps = queryWith (runExceptT . yamlComps)

queryNominals :: AppT IO [Nominal]
queryNominals = queryWith (runExceptT . yamlNominals)

queryTasks :: AppT IO [Task Double]
queryTasks = queryWith (runExceptT . yamlTasks)

queryPilots :: AppT IO [[Pilot]]
queryPilots = queryWith (runExceptT . yamlPilots)

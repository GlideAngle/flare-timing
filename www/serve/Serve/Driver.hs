{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.Driver (driverRun) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant
    ( (:<|>)(..)
    , Get, JSON, Server, Handler, Proxy(..)
    , (:>)
    , err400, errBody, enter, serve, throwError, runReaderTNat
    )
import System.IO
import Control.Monad.Reader (ReaderT, ask, liftIO)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.Directory (doesFileExist)
import System.FilePath (FilePath)
import Data.Yaml (decodeEither)
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
    "tasks" :> Get '[JSON] [Task]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

newtype AppEnv = AppEnv { path :: FilePath }
type FsdbHandler = ReaderT AppEnv Handler

flareTimingApi :: Proxy FlareTimingApi
flareTimingApi = Proxy

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
            runSettings settings =<< mkTaskApp (AppEnv path)

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkTaskApp :: AppEnv -> IO Application
mkTaskApp env = do
    let sc = serverComp env
    let st = serverTask env
    return $ simpleCors $ serve flareTimingApi $ sc :<|> st

-- NOTE: Transforming FsdbHandler :~> Handler with runReaderTNat.
-- SEE: https://kseo.github.io/posts/2017-01-18-natural-transformations-in-servant.html
serverComp :: AppEnv -> Server CompApi
serverComp env =
    enter (runReaderTNat env) queryComps
    :<|> enter (runReaderTNat env) queryNominals

serverTask :: AppEnv -> Server TaskApi
serverTask env =
    enter (runReaderTNat env) queryTasks
    :<|> enter (runReaderTNat env) queryPilots

queryWith :: (FilePath -> IO (Either String a)) -> FsdbHandler a
queryWith f = do
    path' <- path <$> ask
    xs <- liftIO $ f path'
    case xs of
      Left msg -> throwError $ err400 { errBody = LBS.pack msg }
      Right xs' -> return xs'

yamlComps :: FilePath -> ExceptT String IO [Comp]
yamlComps yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither contents of
        Left msg -> throwE msg
        Right CompSettings{..} -> ExceptT . return $ Right [comp]

yamlNominals :: FilePath -> ExceptT String IO [Nominal]
yamlNominals yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither contents of
        Left msg -> throwE msg
        Right CompSettings{..} -> ExceptT . return $ Right [nominal]

yamlTasks :: FilePath -> ExceptT String IO [Task]
yamlTasks yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither contents of
        Left msg -> throwE msg
        Right CompSettings{..} -> ExceptT . return $ Right tasks

yamlPilots :: FilePath -> ExceptT String IO [[Pilot]]
yamlPilots yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither contents of
        Left msg -> throwE msg
        Right CompSettings{..} -> ExceptT . return $ Right $ (fmap . fmap) pilot pilots
    where
        pilot (PilotTrackLogFile p _) = p

queryComps :: FsdbHandler [Comp]
queryComps = queryWith (runExceptT . yamlComps)

queryNominals :: FsdbHandler [Nominal]
queryNominals = queryWith (runExceptT . yamlNominals)

queryTasks :: FsdbHandler [Task]
queryTasks = queryWith (runExceptT . yamlTasks)

queryPilots :: FsdbHandler [[Pilot]]
queryPilots = queryWith (runExceptT . yamlPilots)

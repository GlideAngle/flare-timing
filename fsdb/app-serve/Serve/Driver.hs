{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.Driver (driverRun) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant
import Servant (Get, JSON, Server, Handler, Proxy(..), (:>), serve, throwError)
import System.IO
import Control.Monad.Reader (ReaderT, ask, liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.Directory (doesFileExist)
import System.FilePath (FilePath)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task)
import Data.Flight.Comp (Comp)
import Data.Flight.Nominal (Nominal)
import qualified Data.Flight.Comp as C (parse)
import qualified Data.Flight.Nominal as N (parse)
import qualified Data.Flight.Waypoint as W (parse)
import Data.Flight.Pilot (Pilot(..), parseNames)

type FlareTimingApi = CompApi :<|> TaskApi

type CompApi =
    "comps" :> Get '[JSON] [Comp]
    :<|> "nominals" :> Get '[JSON] [Nominal]

type TaskApi =
    "tasks" :> Get '[JSON] [Task]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

data AppEnv = AppEnv { path :: FilePath }
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
        else putStrLn "Couldn't find the flight score competition database input file."
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

readWith :: (String -> IO (Either String a)) -> FilePath -> IO (Either String a)
readWith f path = do
    contents <- readFile path
    let xml = dropWhile (/= '<') contents
    f xml

queryWith :: (FilePath -> IO (Either String a)) -> FsdbHandler a
queryWith f = do
    path' <- path <$> ask
    xs <- liftIO $ f path'
    case xs of
      Left msg -> throwError $ err400 { errBody = LBS.pack msg }
      Right xs' -> return xs'

readComps :: FilePath -> IO (Either String [Comp])
readComps = readWith C.parse

queryComps :: FsdbHandler [Comp]
queryComps = queryWith readComps

readTasks :: FilePath -> IO (Either String [Task])
readTasks = readWith W.parse

queryTasks :: FsdbHandler [Task]
queryTasks = queryWith readTasks

readPilots :: FilePath -> IO (Either String [[Pilot]])
readPilots = readWith parseNames

queryPilots :: FsdbHandler [[Pilot]]
queryPilots = queryWith readPilots

readNominals :: FilePath -> IO (Either String [Nominal])
readNominals = readWith N.parse

queryNominals :: FsdbHandler [Nominal]
queryNominals = queryWith readNominals

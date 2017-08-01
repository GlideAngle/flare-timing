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
import System.FilePath (FilePath, takeFileName)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task)
import Data.Flight.Comp (Comp)
import qualified Data.Flight.Comp as C (parse)
import qualified Data.Flight.Waypoint as W (parse)
import Data.Flight.Pilot (Pilot(..), parseNames)

type FlareTimingApi = CompApi :<|> TaskApi

type CompApi = "comps" :> Get '[JSON] [Comp]

type TaskApi =
    "tasks" :> Get '[JSON] [Task]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

data AppEnv = AppEnv { path :: FilePath }
type CompHandler = ReaderT AppEnv Handler

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

        go path = do
            putStrLn $ takeFileName path
            contents <- readFile path
            let xml = dropWhile (/= '<') contents

            ts <- W.parse xml
            ps <- parseNames xml
            case (ts, ps) of
                (Left msg, _) -> print msg
                (_, Left msg) -> print msg
                (Right ts', Right ps') ->
                    runSettings settings =<< mkTaskApp (AppEnv path) ts' ps'

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkTaskApp :: AppEnv -> [Task] -> [[Pilot]] -> IO Application
mkTaskApp env ts ps = do
    let sc = serverComp env
    let st = serverTask ts ps
    return $ simpleCors $ serve flareTimingApi $ sc :<|> st

serverTask :: [Task] -> [[Pilot]] -> Server TaskApi
serverTask ts ps = getTasks ts :<|> getPilots ps

getTasks :: [Task] -> Handler [Task]
getTasks = return

getPilots :: [[Pilot]] -> Handler [[Pilot]]
getPilots = return

-- NOTE: Transforming CompHandler :~> Handler with runReaderTNat.
-- SEE: https://kseo.github.io/posts/2017-01-18-natural-transformations-in-servant.html
serverComp :: AppEnv -> Server CompApi
serverComp env =
    enter (runReaderTNat env) queryComps

readComps :: FilePath -> IO (Either String [Comp])
readComps path = do
    contents <- readFile path
    let xml = dropWhile (/= '<') contents
    C.parse xml

queryComps :: CompHandler [Comp]
queryComps = do
    path' <- path <$> ask
    cs <- liftIO $ readComps path'
    case cs of
      Left msg -> throwError $ err400 { errBody = LBS.pack msg }
      Right cs' -> return cs'

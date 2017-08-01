{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Serve.Driver (driverRun) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant
import Servant (Get, JSON, Server, Handler, Proxy(..), (:>), serve)
import System.IO
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import System.Directory (doesFileExist)
import System.FilePath (takeFileName)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task)
import Data.Flight.Comp (Comp)
import qualified Data.Flight.Comp as C (parse)
import qualified Data.Flight.Waypoint as W (parse)
import Data.Flight.Pilot (Pilot(..), parseNames)

type TaskApi =
    "comps" :> Get '[JSON] [Comp]
    :<|> "tasks" :> Get '[JSON] [Task]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

taskApi :: Proxy TaskApi
taskApi = Proxy

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

            cs <- C.parse xml
            ts <- W.parse xml
            ps <- parseNames xml
            case (cs, ts, ps) of
                (Left msg, _, _) -> print msg
                (_, Left msg, _) -> print msg
                (_, _, Left msg) -> print msg
                (Right cs', Right ts', Right ps') ->
                    runSettings settings =<< mkApp cs' ts' ps'

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: [Comp] -> [Task] -> [[Pilot]] -> IO Application
mkApp cs ts ps = return $ simpleCors $ serve taskApi $ server cs ts ps

server :: [Comp] -> [Task] -> [[Pilot]] -> Server TaskApi
server cs ts ps =
    getComps cs
    :<|> getTasks ts
    :<|> getPilots ps

getComps :: [Comp] -> Handler [Comp]
getComps cs = do
    void . liftIO . print $ "COMPS: " ++ show cs
    return cs

getTasks :: [Task] -> Handler [Task]
getTasks ts = do
    void . liftIO . print $ "TASKS: " ++ show ts
    return ts

getPilots :: [[Pilot]] -> Handler [[Pilot]]
getPilots ps = do
    void . liftIO . print $ "PILOTS: " ++ show ps
    return ps

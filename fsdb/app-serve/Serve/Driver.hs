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

import System.Directory (doesFileExist)
import System.FilePath (takeFileName)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task)
import Data.Flight.Comp (Comp)
import qualified Data.Flight.Comp as C (parse)
import qualified Data.Flight.Waypoint as W (parse)

type TaskApi =
    "tasks" :> Get '[JSON] [Task]
    :<|> "comps" :> Get '[JSON] [Comp]

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

            comp <- C.parse xml
            tasks <- W.parse xml
            case (comp, tasks) of
                (Left msg, _) -> print msg
                (_, Left msg) -> print msg
                (Right comp', Right tasks') ->
                    runSettings settings =<< mkApp comp' tasks'

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: [Comp] -> [Task] -> IO Application
mkApp comp xs = return $ simpleCors $ serve taskApi $ server comp xs

server :: [Comp] -> [Task] -> Server TaskApi
server comp xs =
    getTasks xs
    :<|> getComps comp

getTasks :: [Task] -> Handler [Task]
getTasks = return

getComps :: [Comp] -> Handler [Comp]
getComps = return

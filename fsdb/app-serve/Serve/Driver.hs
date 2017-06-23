{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Serve.Driver (driverRun) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant
import System.IO

import System.Directory (doesFileExist)
import System.FilePath (takeFileName)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task)
import Data.Flight.Waypoint (parse)

type TaskApi = "tasks" :> Get '[JSON] [Task]

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

            p <- parse $ dropWhile (/= '<') contents
            case p of
                 Left msg -> print msg
                 Right tasks -> runSettings settings =<< mkApp tasks

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: [Task] -> IO Application
mkApp xs = return $ simpleCors $ serve taskApi $ server xs

server :: [Task] -> Server TaskApi
server = getTasks

getTasks :: [Task] -> Handler [Task]
getTasks = return

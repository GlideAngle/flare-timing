{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Serve.Driver (driverRun) where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Data.Ratio ((%))

import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Serve.Args (withCmdArgs)
import Serve.Options (ServeOptions(..))
import Data.Flight.Types (Task(..), Turnpoint(..))

type TaskApi = "tasks" :> Get '[JSON] [Task]

taskApi :: Proxy TaskApi
taskApi = Proxy

driverRun :: IO ()
driverRun = withCmdArgs drive

drive :: ServeOptions -> IO ()
drive _ = do
    runSettings settings =<< mkApp
    where
        port = 3000

        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings

mkApp :: IO Application
mkApp = return $ serve taskApi server

server :: Server TaskApi
server = getTasks

getTasks :: Handler [Task]
getTasks = return [ Task "Day 1" (Just (2,5)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 10000
                                              , Turnpoint "PINEY" ((-2382244465829709) % 70368744177664) (5205244616347379 % 35184372088832) 400
                                              , Turnpoint "EUGOWR" ((-2353109694677831) % 70368744177664) (1304859322090143 % 8796093022208) 400
                                              , Turnpoint "GOALD1" ((-4731545100385203) % 140737488355328) (1305427285816587 % 8796093022208) 400
                                              ]
                  , Task "day 2" (Just (2,5)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 5000
                                              , Turnpoint "MINES" ((-1157644302872331) % 35184372088832) (1301689122204009 % 8796093022208) 400
                                              , Turnpoint "SKULLS" ((-4637806896266137) % 140737488355328) (648951861785951 % 4398046511104) 400
                                              , Turnpoint "BOGAN" ((-4662058780259527) % 140737488355328) (5204135604939139 % 35184372088832) 400
                                              ]
                  , Task "Day 3" (Just (2,5)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 25000
                                              , Turnpoint "TYAGON" ((-4788039942960799) % 140737488355328) (1303785934858643 % 8796093022208) 400
                                              , Turnpoint "CROW" ((-4801674590832663) % 140737488355328) (5224924994875267 % 35184372088832) 400
                                              , Turnpoint "BOOKHA" ((-4900756597384581) % 140737488355328) (5230699805866207 % 35184372088832) 1000
                                              ]
                  , Task "Day 4" (Just (2,4)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 15000
                                              , Turnpoint "MINES" ((-1157644302872331) % 35184372088832) (1301689122204009 % 8796093022208) 25000
                                              , Turnpoint "GOALD4" ((-4568849749096677) % 140737488355328) (5242084413142991 % 35184372088832) 400
                                              ]
                  , Task "Day 5" (Just (2,4)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 15000
                                              , Turnpoint "TOMING" ((-572908538097335) % 17592186044416) (5215258792331303 % 35184372088832) 5000
                                              , Turnpoint "DUNEDO" ((-4505907722179523) % 140737488355328) (328608027531553 % 2199023255552) 400
                                              ]
                  , Task "Day 6" (Just (2,4)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 15000
                                              , Turnpoint "BUMF" ((-2265520311425009) % 70368744177664) (2599459760888485 % 17592186044416) 5000
                                              , Turnpoint "GOALD6" ((-1115106397016933) % 35184372088832) (5217709735691011 % 35184372088832) 400
                                              ]
                  , Task "Day 7" (Just (2,5)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 10000
                                              , Turnpoint "SKULLS" ((-4637806896266137) % 140737488355328) (648951861785951 % 4398046511104) 5000
                                              , Turnpoint "YEOVAL" ((-2305316651007245) % 70368744177664) (5230142133568599 % 35184372088832) 400
                                              , Turnpoint "DAY7GO" ((-2317654402923915) % 70368744177664) (5233656700496553 % 35184372088832) 400
                                              ]
                  , Task "Day 8" (Just (2,5)) [ Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 100
                                              , Turnpoint "FORBES" ((-4695195421892789) % 140737488355328) (2602448497375571 % 17592186044416) 10000
                                              , Turnpoint "MARSDE" ((-4750372961577379) % 140737488355328) (5190702915363065 % 35184372088832) 5000
                                              , Turnpoint "YARRAB" ((-2331251755361365) % 70368744177664) (5192271434670785 % 35184372088832) 400
                                              , Turnpoint "DAY8GO" ((-4695143349022097) % 140737488355328) (5204876939659051 % 35184372088832) 400
                                              ]
                  ]

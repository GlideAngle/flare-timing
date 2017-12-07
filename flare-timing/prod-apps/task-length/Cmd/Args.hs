{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Args
    ( Drive(..)
    , withCmdArgs
    ) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , help
    , typ
    , opt
    , explicit
    , name
    , groupname
    , cmdArgs
    , (&=)
    )
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.RawString.QQ (r)
import Cmd.Options (CmdOptions(..))
import Flight.TaskTrack (TaskDistanceMeasure(..))

description :: String
description = intro
    where
        intro = [r|
Works out the task length by following an optimal route.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.comp-inputs.yaml
    Writes ./c.task-length.yaml
|]

data Drive
    = Drive { dir :: String
            , file :: String
            , task :: [Int]
            , measure :: TaskDistanceMeasure
            , noTaskWaypoints :: Bool
            }
    deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { dir = def
          &= help "Over all the competition *.comp.yaml files in this directory"
          &= groupname "Source"

          , file = def
          &= help "With this one competition *.comp.yaml file"
          &= groupname "Source"

          , task = def
          &= help "Which tasks?"
          &= typ "TASK NUMBER"
          &= opt "name"
          &= groupname "Filter"

          , measure = def
          &= help "Which way to measure task distances, taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges"
          &= typ "METHOD"
          &= groupname "Filter"

          , noTaskWaypoints = def
          &= help "Exclude the task waypoints?"
          &= explicit
          &= name "no-task-waypoints"
          &= groupname "Filter"
          }
          &= summary description
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

cmdArgsToDriveArgs :: Drive -> Maybe CmdOptions
cmdArgsToDriveArgs Drive{..} =
    return CmdOptions { dir = dir
                      , file = file
                      , task = task
                      , measure = measure
                      , noTaskWaypoints = noTaskWaypoints
                      }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkedOptions :: CmdOptions -> IO (Maybe String)
checkedOptions CmdOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")

    return $ either Just (const Nothing) x

withCmdArgs :: (CmdOptions -> IO ()) -> IO ()
withCmdArgs f = do
    ca <- run
    case cmdArgsToDriveArgs ca of
        Nothing -> putStrLn "Couldn't parse args."
        Just o -> do
            checked <- checkedOptions o
            case checked of
                Just s -> putStrLn s
                Nothing -> f o

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
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
        intro = [r|Given a competition input YAML file, *.comp-input.yaml, work out the task length by flowing an optimal route.
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
checkedOptions :: CmdOptions -> IO (Either String CmdOptions)
checkedOptions o@CmdOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")
    case x of
         Left s -> return $ Left s
         Right _ -> return $ Right o

withCmdArgs :: (CmdOptions -> IO ()) -> IO ()
withCmdArgs f = do
    ca <- run
    print ca
    case cmdArgsToDriveArgs ca of
        Nothing -> putStrLn "Couldn't parse args."
        Just o -> do
            print o
            checked <- checkedOptions o
            case checked of
                Left s -> putStrLn s
                Right co -> f co

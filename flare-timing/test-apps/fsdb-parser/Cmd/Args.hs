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
    , groupname
    , cmdArgs
    , (&=)
    )
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.RawString.QQ (r)
import Cmd.Options (CmdOptions(..), Detail(..))

description :: String -> String
description programName =
    intro ++ programName ++ about
    where
        intro =
            [r|Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding Commission) is an Air Sport Commission (ASC) of the Fédération Internationale Aéronautique (FAI). CIVL produce FS, the official software for scoring hang gliding and paragliding competitions. FSDB is the database of FS, an XML format for inputs, working and outputs of scoring.

|]

        about =
            [r| is a parser for a subset of the FSDB, just enough to cover the inputs of scoring.
|]

data Drive
    = Drive { dir :: String
            , file :: String
            , detail :: [Detail]
            }
    deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { dir = def
          &= help "Over all the FSDB files in this directory"
          &= groupname "Source"

          , file = def
          &= help "With this one FSDB file"
          &= groupname "Source"

          , detail = def
          &= help "Focus on these details"
          &= typ "tasks | nominals"
          &= groupname "Filter"
          }
          &= summary (description programName)
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

cmdArgsToDriveArgs :: Drive -> Maybe CmdOptions
cmdArgsToDriveArgs Drive{..} =
    return CmdOptions { dir = dir, file = file, detail = detail }

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

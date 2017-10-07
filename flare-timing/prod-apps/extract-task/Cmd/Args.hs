{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
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
    , groupname
    , cmdArgs
    , (&=)
    )
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Cmd.Options (CmdOptions(..))

description :: String
description = "Convert a competition FSDB (XML) file to YAML with only the inputs needed for scoring."

data Drive
    = Drive { dir :: String
            , file :: String
            }
    deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { dir = def
          &= help "Over all the competition FSDB files in this directory"
          &= groupname "Source"

          , file = def
          &= help "With this one competition FSDB file"
          &= groupname "Source"
          }
          &= summary description
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

cmdArgsToDriveArgs :: Drive -> Maybe CmdOptions
cmdArgsToDriveArgs Drive{..} =
    return CmdOptions { dir = dir, file = file }

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
    case cmdArgsToDriveArgs ca of
        Nothing -> putStrLn "Couldn't parse args."
        Just o -> do
            checked <- checkedOptions o
            case checked of
                Left s -> putStrLn s
                Right co -> f co

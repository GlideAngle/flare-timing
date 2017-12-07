{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Igc.Args
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
    , cmdArgs
    , (&=)
    )
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.RawString.QQ (r)
import Igc.Options (IgcOptions(..))

description :: String
description = [r|A parser for IGC, a plain-text file format from the International Gliding Commission for recording flights.
|]

data Drive
    = Drive { dir :: String
            , file :: String
            }
    deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { dir = def &= help "Over all the IGC files in this directory"
          , file = def &= help "With this one IGC file"
          }
          &= summary description
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

cmdArgsToDriveArgs :: Drive -> Maybe IgcOptions
cmdArgsToDriveArgs Drive{ dir = d, file = f } =
    return IgcOptions { dir = d, file = f }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkedOptions :: IgcOptions -> IO (Maybe String)
checkedOptions IgcOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")

    return $ either Just (const Nothing) x

withCmdArgs :: (IgcOptions -> IO ()) -> IO ()
withCmdArgs f = do
    ca <- run
    case cmdArgsToDriveArgs ca of
        Nothing -> putStrLn "Couldn't parse args."
        Just o -> do
            checked <- checkedOptions o
            case checked of
                Just s -> putStrLn s
                Nothing -> f o

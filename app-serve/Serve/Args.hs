{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Serve.Args
    ( Drive(..)
    , dryRunCmdArgs
    , withCmdArgs
    ) where

import Paths_flight_fsdb (version)
import Data.Version (showVersion)
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
import System.Directory (doesFileExist)
import Text.RawString.QQ (r)
import Serve.Options (ServeOptions(..))

description :: String
description = intro
    where
        intro = [r|

Serving tasks from flight fsdb files.
|]

data Drive = Drive { file :: String } deriving (Show, Data, Typeable)

drive :: Drive
drive
    = Drive { file = def &= help "With this one file"
            }
            &= summary ("Serving Tasks from Flight Scoring Database Files" ++ showVersion version ++ description)
            &= program "flight-fsdb-serve.exe"

run :: IO Drive
run = cmdArgs drive

cmdArgsToDriveArgs :: Drive -> Maybe ServeOptions
cmdArgsToDriveArgs Drive{ file = f } =
    return ServeOptions { file = f }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkedOptions :: ServeOptions -> IO (Either String ServeOptions)
checkedOptions o@ServeOptions{..} = do
    x <- runExceptT $ do
        when (file == "") (throwError "No --file argument")

        dfe <- liftIO $ doesFileExist file
        unless dfe (throwError
               "The --file argument is not a file")
    case x of
         Left s -> return $ Left s
         Right _ -> return $ Right o

dryRunCmdArgs :: IO ()
dryRunCmdArgs = print =<< run

withCmdArgs :: (ServeOptions -> IO ()) -> IO ()
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

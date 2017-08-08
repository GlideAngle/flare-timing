{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Args
    ( Drive(..)
    , dryRunCmdArgs
    , withCmdArgs
    ) where

import Paths_flare_timing (version)
import Data.Version (showVersion)
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
import Text.RawString.QQ (r)
import Cmd.Options (CmdOptions(..))

description :: String
description = intro
    where
        intro = [r|

Convert fsdb (XML) to yaml with only the inputs needed for scoring.
|]

data Drive
    = Drive { dir :: String
            , file :: String
            }
    deriving (Show, Data, Typeable)

drive :: Drive
drive
    = Drive { dir = def
            &= help "Over all the competition *.comp.yaml files in this directory"
            &= groupname "Source"

            , file = def
            &= help "With this one competition *.comp.yaml file"
            &= groupname "Source"
            }
            &= summary ("Flight Scoring Track Zone Intersect Checker " ++ showVersion version ++ description)
            &= program "flight-track-intersect-zone.exe"

run :: IO Drive
run = cmdArgs drive

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

dryRunCmdArgs :: IO ()
dryRunCmdArgs = print =<< run

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

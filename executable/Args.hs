{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Args
    ( Drive(..)
    , dryRunCmdArgs
    , withCmdArgs
    , contributorAcks
    , contributorCopyrightNotices
    ) where

import Paths_flight_igc (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.RawString.QQ (r)
import EmbedString (embedStr)
import Options (DriveOptions(..))

contributorAcks :: String
contributorAcks = [r|
Copyright Acknowledgements
--------------------------
Timing is Copyright Neil Mitchell 2011-2014. All rights reserved.
|]

contributorCopyrightNotices :: String
contributorCopyrightNotices = intro ++ copyrights
    where
        copyrights = $(embedStr (readFile "./executable/Shake/LICENSE"))
        intro = [r|
Included Software Copyright Notices
-----------------------------------
The LICENSE for Shake from which the Timing module was taken ...
SOURCE: https://github.com/ndmitchell/shake/blob/master/LICENSE

|]

description :: String
description = intro ++ contributorAcks ++ contributorCopyrightNotices
    where
        intro = [r|

Parsing flight IGC files.
|]

data Drive
    = Drive { dir :: String
            , file :: String
            }
    deriving (Show, Data, Typeable)

drive :: Drive
drive
    = Drive { dir = def &= help "Over all the files in this directory"
            , file = def &= help "With this one file"
            }
            &= summary ("Flight IGC Parser " ++ showVersion version ++ description)
            &= program "flight-igc.exe"

run :: IO Drive
run = cmdArgs drive

cmdArgsToDriveArgs :: Drive -> Maybe DriveOptions
cmdArgsToDriveArgs Drive{ dir = d, file = f } = do
    return $ DriveOptions { dir = d, file = f }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkedOptions :: DriveOptions -> IO (Either String DriveOptions)
checkedOptions o@DriveOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError $ "The --dir argument is not a directory or the --file argument is not a file")
    case x of
         Left s -> return $ Left s
         Right _ -> return $ Right o

dryRunCmdArgs :: IO ()
dryRunCmdArgs = print =<< run

withCmdArgs :: (DriveOptions -> IO ()) -> IO ()
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

{-# LANGUAGE RecordWildCards #-}

module Kml.Args (withCmdArgs) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Kml.Options (KmlOptions(..), mkOptions)

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkOptions :: KmlOptions -> IO (Maybe String)
checkOptions KmlOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")

    return $ either Just (const Nothing) x

withCmdArgs :: (KmlOptions -> IO ()) -> IO ()
withCmdArgs f = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    checked <- checkOptions options
    case checked of
        Just s -> putStrLn s
        Nothing -> f options

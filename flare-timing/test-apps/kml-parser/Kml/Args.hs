{-# LANGUAGE RecordWildCards #-}

module Kml.Args (checkOptions) where

import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import Kml.Options (KmlOptions(..))

checkOptions :: KmlOptions -> IO (Maybe String)
checkOptions KmlOptions{..} = do

    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")

    return $ either Just (const Nothing) x

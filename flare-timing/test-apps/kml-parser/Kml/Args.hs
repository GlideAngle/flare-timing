{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Kml.Args
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
import Kml.Options (KmlOptions(..))

description :: String
description = "A parser of KML, the Keyhole Markup Language, an XML format."

data Drive
    = Drive { dir :: String
            , file :: String
            }
    deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { dir = def &= help "Over all the KML files in this directory"
          , file = def &= help "With this one KML file"
          }
          &= summary description
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

cmdArgsToDriveArgs :: Drive -> Maybe KmlOptions
cmdArgsToDriveArgs Drive{ dir = d, file = f } =
    return KmlOptions { dir = d, file = f }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkedOptions :: KmlOptions -> IO (Either String KmlOptions)
checkedOptions o@KmlOptions{..} = do
    x <- runExceptT $ do
        when (dir == "" && file == "") (throwError "No --dir or --file argument")

        dfe <- liftIO $ doesFileExist file
        dde <- liftIO $ doesDirectoryExist dir
        unless (dfe || dde) (throwError
               "The --dir argument is not a directory or the --file argument is not a file")
    case x of
         Left s -> return $ Left s
         Right _ -> return $ Right o

withCmdArgs :: (KmlOptions -> IO ()) -> IO ()
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

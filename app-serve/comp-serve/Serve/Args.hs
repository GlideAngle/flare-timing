{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Serve.Args
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
import System.Directory (doesFileExist)
import Serve.Options (ServeOptions(..))

description :: String
description = "Serve nominals, tasks and pilots from a competition YAML file."

newtype Drive = Drive { file :: String } deriving (Show, Data, Typeable)

drive :: String -> Drive
drive programName =
    Drive { file = def
          &= help "With one competition *.comp.yaml file supplied"
          &= groupname "Source"
          }
          &= summary description
          &= program programName

run :: IO Drive
run = do
    s <- getProgName
    cmdArgs $ drive s

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

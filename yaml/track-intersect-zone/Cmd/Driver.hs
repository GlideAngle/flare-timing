{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName, replaceExtension)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Data.Yaml (decodeEither)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import Data.Flight.Comp (CompSettings(..))

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp.yaml") dir
            mapM_ go files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        go yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath
            let yamlTrackPath = replaceExtension yamlCompPath ".track.yaml"

            settings <- runExceptT $ yamlCompSettings yamlCompPath
            case settings of
                Left msg -> print msg
                Right cfg -> do
                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                cfg

                    BS.writeFile yamlTrackPath yaml

        cmp a b =
            case (a, b) of
                _ -> compare a b

yamlCompSettings :: FilePath -> ExceptT String IO CompSettings
yamlCompSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

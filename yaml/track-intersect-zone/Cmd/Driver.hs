{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Data.Yaml (decodeEither)
import Data.Number.RoundingFunctions (dpRound)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import qualified Data.Flight.Comp as C
import Data.Flight.TrackZone (TrackZoneIntersect(..), TaskTrack(..))
import qualified Flight.Task as FT
import Flight.Task
    ( LatLng(..)
    , Lat(..)
    , Lng(..)
    , Radius(..)
    , Zone(..)
    , TaskDistance(..)
    , DistancePath(..)
    , Tolerance(..)
    , EdgeDistance(..)
    )

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
            let yamlTrackPath =
                    flip replaceExtension ".track.yaml"
                    $ dropExtension yamlCompPath

            intersects <- runExceptT $ analyze yamlCompPath
            case intersects of
                Left msg -> print msg
                Right xs -> do
                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                xs

                    BS.writeFile yamlTrackPath yaml

        cmp a b =
            case (a, b) of
                _ -> compare a b

readSettings :: FilePath -> ExceptT String IO C.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

followTracks :: C.CompSettings -> ExceptT String IO TrackZoneIntersect
followTracks C.CompSettings{..} = do
    ExceptT . return . Right $
        TrackZoneIntersect
            { taskTracks = taskTrack <$> tasks
            }

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

analyze :: FilePath -> ExceptT String IO TrackZoneIntersect
analyze compYamlPath = do
    settings <- readSettings compYamlPath
    followTracks settings

taskTrack :: C.Task -> TaskTrack
taskTrack C.Task{..} =
    TaskTrack { distancePointToPoint = toKm ptd
              , distanceEdgeToEdge = toKm etd
              }
    where
        zs :: [Zone]
        zs = f <$> zones

        ptd :: TaskDistance
        ptd = FT.distancePointToPoint zs

        etd :: TaskDistance
        etd = edges $ FT.distanceEdgeToEdge PathPointToZone mm30 zs

        toKm :: TaskDistance -> Double
        toKm (TaskDistance d) =
            fromRational $ dpRound 3 dKm
            where 
                MkQuantity dKm = convert d :: Quantity Rational [u| km |]

        f C.Zone{..} =
            Cylinder
                (Radius (MkQuantity $ radius % 1))
                (LatLng (Lat latRad, Lng lngRad))
            where
                C.Latitude lat' = lat
                C.Longitude lng' = lng

                latDeg = MkQuantity lat' :: Quantity Rational [u| deg |]
                lngDeg = MkQuantity lng' :: Quantity Rational [u| deg |]

                latRad = convert latDeg :: Quantity Rational [u| rad |]
                lngRad = convert lngDeg :: Quantity Rational [u| rad |]

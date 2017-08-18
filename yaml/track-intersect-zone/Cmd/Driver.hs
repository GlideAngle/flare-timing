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
import qualified Data.Flight.Comp as Cmp
import qualified Data.Flight.TrackZone as Z 
import Data.Flight.TrackZone
    ( TrackZoneIntersect(..)
    , TaskTrack(..)
    , TrackLine(..)
    , FlownTrack(..)
    , PilotFlownTrack(..)
    )
import qualified Flight.Task as Tsk
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
    , center
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
                ("pointToPoint", _) -> LT
                ("edgeToEdge", _) -> GT
                ("lat", _) -> LT
                ("lng", _) -> GT
                ("distance", _) -> LT
                ("wayPoints", _) -> GT
                ("taskTracks", _) -> LT
                ("pilotTracks", _) -> GT
                ("launched", _) -> LT
                ("madeGoal", "launched") -> GT
                ("madeGoal", _) -> LT
                ("zonesMade", "launched") -> GT
                ("zonesMade", "madeGoal") -> GT
                ("zonesMade", _) -> LT
                ("zonesNotMade", "launched") -> GT
                ("zonesNotMade", "madeGoal") -> GT
                ("zonesNotMade", "zonesMade") -> GT
                ("zonesNotMade", _) -> LT
                ("timeToGoal", "launched") -> GT
                ("timeToGoal", "madeGoal") -> GT
                ("timeToGoal", "zonesMade") -> GT
                ("timeToGoal", "zonesNotMade") -> GT
                ("timeToGoal", _) -> LT
                ("distanceToGoal", "launched") -> GT
                ("distanceToGoal", "madeGoal") -> GT
                ("distanceToGoal", "zonesMade") -> GT
                ("distanceToGoal", "zonesNotMade") -> GT
                ("distanceToGoal", "timeToGoal") -> GT
                ("distanceToGoal", _) -> LT
                ("bestDistance", _) -> GT
                _ -> compare a b

readSettings :: FilePath -> ExceptT String IO Cmp.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

followTracks :: Cmp.CompSettings -> ExceptT String IO TrackZoneIntersect
followTracks Cmp.CompSettings{..} = do
    ExceptT . return . Right $
        TrackZoneIntersect
            { taskTracks = taskTrack <$> tasks
            , pilotTracks = (fmap . fmap) pilotTrack pilots
            }

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

analyze :: FilePath -> ExceptT String IO TrackZoneIntersect
analyze compYamlPath = do
    settings <- readSettings compYamlPath
    followTracks settings

taskTrack :: Cmp.Task -> TaskTrack
taskTrack Cmp.Task{..} =
    TaskTrack { pointToPoint =
                  TrackLine
                      { distance = toKm ptd
                      , waypoints = wpPoint
                      }
              , edgeToEdge =
                  TrackLine
                      { distance = toKm etd
                      , waypoints = wpEdge
                      }
              }
    where
        zs :: [Zone]
        zs = toCylinder <$> zones

        ptd :: TaskDistance
        ptd = Tsk.distancePointToPoint zs

        wpPoint :: [Z.LatLng]
        wpPoint =
            convertLatLng <$> ps
            where
                ps = center <$> zs

        ed :: EdgeDistance
        ed = Tsk.distanceEdgeToEdge PathPointToZone mm30 zs

        etd :: TaskDistance
        etd = edges ed

        wpEdge :: [Z.LatLng]
        wpEdge =
            convertLatLng <$> xs
            where
                xs = edgeLine ed

        toKm :: TaskDistance -> Double
        toKm (TaskDistance d) =
            fromRational $ dpRound 3 dKm
            where 
                MkQuantity dKm = convert d :: Quantity Rational [u| km |]

convertLatLng :: LatLng [u| rad |] -> Z.LatLng
convertLatLng (LatLng (Lat eLat, Lng eLng)) =
    Z.LatLng { lat = Cmp.Latitude eLat'
             , lng = Cmp.Longitude eLng'
             }
    where
        MkQuantity eLat' =
            convert eLat :: Quantity Rational [u| deg |]

        MkQuantity eLng' =
            convert eLng :: Quantity Rational [u| deg |]

toCylinder :: Cmp.Zone -> Zone
toCylinder Cmp.Zone{..} =
    Cylinder
        (Radius (MkQuantity $ radius % 1))
        (LatLng (Lat latRad, Lng lngRad))
    where
        Cmp.Latitude lat' = lat
        Cmp.Longitude lng' = lng

        latDeg = MkQuantity lat' :: Quantity Rational [u| deg |]
        lngDeg = MkQuantity lng' :: Quantity Rational [u| deg |]

        latRad = convert latDeg :: Quantity Rational [u| rad |]
        lngRad = convert lngDeg :: Quantity Rational [u| rad |]

pilotTrack :: Cmp.PilotTrackLogFile -> PilotFlownTrack
pilotTrack (Cmp.PilotTrackLogFile pilot _) =
    PilotFlownTrack pilot (Just track)
    where
        track = FlownTrack { launched = True
                           , madeGoal = True
                           , zonesMade = [1,2]
                           , zonesNotMade = [3,4,5]
                           , timeToGoal = Nothing
                           , distanceToGoal = 0
                           , bestDistance = 0
                           }

module FlareTiming.Comms
    ( GetConstraint
    , getComps
    , getNominals
    , getTasks
    , getTaskLengths
    , getPilots
    , getPilotsStatus
    , getValidity
    , getAllocation
    , getTaskScore
    , getTaskValidityWorking
    , getTaskLengthSphericalEdge
    , getTaskLengthEllipsoidEdge
    , getTaskLengthProjectedEdgeSpherical
    , getTaskLengthProjectedEdgeEllipsoid
    , getTaskLengthProjectedEdgePlanar
    , getTaskPilotDnf
    , getTaskPilotNyp
    , getTaskPilotDf
    , getTaskPilotTrack
    , emptyRoute
    ) where

import Reflex
import Reflex.Dom
import Data.Aeson (FromJSON)
import qualified Data.Text as T (Text, pack)
import Control.Monad.IO.Class (MonadIO)

import WireTypes.Comp (Comp(..), Nominal(..), Task(..))
import WireTypes.Route
import WireTypes.Pilot (PilotTaskStatus(..), Pilot(..), PilotId(..), getPilotId)
import WireTypes.Point
import WireTypes.Validity
import WireTypes.ValidityWorking
import FlareTiming.Events (IxTask(..))

-- NOTE: Possible alternatives for mapUri ...
-- mapUri s = "http://localhost:3000" <> s
-- mapUri s = "http://1976-never-land.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://1989-lift-lines.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2012-forbes.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2018-forbes.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2018-dalmatian.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "/json" <> s <> ".json"
mapUri :: T.Text -> T.Text
mapUri s = "http://localhost:3000" <> s

emptyRoute :: OptimalRoute (Maybe a)
emptyRoute =
    OptimalRoute
        { taskRoute = Nothing
        , taskRouteSpeedSubset = Nothing
        , speedRoute = Nothing
        }

type GetConstraint t m =
    ( MonadIO (Performable m)
    , HasJSContext (Performable m)
    , PerformEvent t m
    , TriggerEvent t m
    )

req :: T.Text -> Maybe T.Text -> XhrRequest ()
req uri md = XhrRequest "GET" (maybe uri id md) def

getTasks
    :: GetConstraint t m
    => Event t a
    -> m (Event t [Task])
getTasks ev = do
    let u = mapUri "/comp-input/tasks"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskLengths
    :: GetConstraint t m
    => Event t a
    -> m (Event t [TaskDistance])
getTaskLengths ev = do
    let u = mapUri "/task-length"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getComps
    :: GetConstraint t m
    => Event t a
    -> m (Event t Comp)
getComps ev = do
    let u = mapUri "/comp-input/comps"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getNominals
    :: GetConstraint t m
    => Event t a
    -> m (Event t Nominal)
getNominals ev = do
    let u = mapUri "/comp-input/nominals"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getPilots
    :: GetConstraint t m
    => Event t a
    -> m (Event t [Pilot])
getPilots ev = do
    let u = mapUri "/comp-input/pilots"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getPilotsStatus
    :: GetConstraint t m
    => Event t a
    -> m (Event t [(Pilot, [PilotTaskStatus])])
getPilotsStatus ev = do
    let u = mapUri "/gap-point/pilots-status"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getValidity
    :: GetConstraint t m
    => Event t a
    -> m (Event t [Maybe Validity])
getValidity ev = do
    let u = mapUri "/gap-point/validity"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getAllocation
    :: GetConstraint t m
    => Event t a
    -> m (Event t [Maybe Allocation])
getAllocation ev = do
    let u = mapUri "/gap-point/allocation"
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskScore
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t [(Pilot, Breakdown)])
getTaskScore IxTaskNone _ = return never
getTaskScore (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/gap-point/"
            <> (T.pack . show $ ii)
            <> "/score"

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskValidityWorking
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t (Maybe ValidityWorking))
getTaskValidityWorking IxTaskNone _ = return never
getTaskValidityWorking (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/gap-point/"
            <> (T.pack . show $ ii)
            <> "/validity-working"

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskLength_
    :: GetConstraint t m
    => T.Text
    -> IxTask
    -> Event t a
    -> m (Event t (OptimalRoute (Maybe TrackLine)))
getTaskLength_ _ IxTaskNone _ = return never
getTaskLength_ path (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/task-length/"
            <> (T.pack . show $ ii)
            <> "/"
            <> path

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskLengthSphericalEdge, getTaskLengthEllipsoidEdge
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t (OptimalRoute (Maybe TrackLine)))
getTaskLengthSphericalEdge = getTaskLength_ "spherical-edge"
getTaskLengthEllipsoidEdge = getTaskLength_ "ellipsoid-edge"

getTaskLengthProjected_
    :: GetConstraint t m
    => T.Text
    -> IxTask
    -> Event t a
    -> m (Event t (Maybe TrackLine))
getTaskLengthProjected_ _ IxTaskNone _ = return never 
getTaskLengthProjected_ path (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/task-length/"
            <> (T.pack . show $ ii)
            <> "/"
            <> path

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskLengthProjectedEdgeSpherical, getTaskLengthProjectedEdgeEllipsoid
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t (Maybe TrackLine))
getTaskLengthProjectedEdgeSpherical = getTaskLengthProjected_ "projected-edge-spherical"
getTaskLengthProjectedEdgeEllipsoid = getTaskLengthProjected_ "projected-edge-ellipsoid"

getTaskLengthProjectedEdgePlanar
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t (Maybe PlanarTrackLine))
getTaskLengthProjectedEdgePlanar IxTaskNone _ = return never
getTaskLengthProjectedEdgePlanar (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/task-length/"
            <> (T.pack . show $ ii)
            <> "/projected-edge-planar"

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskPilot_
    :: GetConstraint t m
    => T.Text
    -> T.Text
    -> IxTask
    -> Event t a
    -> m (Event t [Pilot])
getTaskPilot_ _ _ IxTaskNone _ = return never
getTaskPilot_ stage path (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/"
            <> stage
            <> "/"
            <> (T.pack . show $ ii)
            <> "/"
            <> path

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getTaskPilotDnf, getTaskPilotNyp, getTaskPilotDf
    :: GetConstraint t m
    => IxTask
    -> Event t a
    -> m (Event t [Pilot])
getTaskPilotDnf = getTaskPilot_ "cross-zone" "pilot-dnf"
getTaskPilotNyp = getTaskPilot_ "cross-zone" "pilot-nyp"
getTaskPilotDf = getTaskPilot_ "gap-point" "pilot-df"

getTaskPilotTrack
    ::
        ( MonadIO (Performable m)
        , HasJSContext (Performable m)
        , PerformEvent t m
        , TriggerEvent t m
        , FromJSON a
        )
   => IxTask
   -> Event t Pilot
   -> m (Event t a)
getTaskPilotTrack IxTaskNone _ = return never
getTaskPilotTrack (IxTask ii) ev = do
    let u :: PilotId -> T.Text
        u (PilotId pid) =
            mapUri
            $ "/pilot-track/"
            <> (T.pack . show $ ii)
            <> "/"
            <> (T.pack pid)

    let req' md = XhrRequest "GET" (u md) def
    rsp <- performRequestAsync . fmap req' $ getPilotId <$> ev
    return $ fmapMaybe decodeXhrResponse rsp

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
    , getTaskPilotAbs
    , getTaskPilotDf
    , getTaskPilotDfNoTrack
    , getTaskPilotTrack
    , getTaskPilotTrackFlyingSection
    , getTaskArrival
    , getTaskLead
    , emptyRoute
    ) where

import Reflex
import Reflex.Dom
import Data.Aeson (FromJSON)
import qualified Data.Text as T (Text, pack)
import Control.Monad.IO.Class (MonadIO)

import WireTypes.Comp (Comp(..), Nominal(..), Task(..))
import WireTypes.Arrival (TrackArrival)
import WireTypes.Lead (TrackLead)
import WireTypes.Route
import WireTypes.Pilot
    ( PilotTaskStatus(..), Pilot(..), PilotId(..)
    , Dnf(..), DfNoTrack(..), Nyp(..)
    , getPilotId
    )
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

type GetConstraint t m a b =
    ( MonadIO (Performable m)
    , HasJSContext (Performable m)
    , PerformEvent t m
    , TriggerEvent t m
    , FromJSON a
    )

req :: T.Text -> Maybe T.Text -> XhrRequest ()
req uri md = XhrRequest "GET" (maybe uri id md) def

get
    :: GetConstraint t m b b
    => T.Text -- ^ the path
    -> Event t a
    -> m (Event t b)
get path ev = do
    let u = mapUri path
    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

getIxTask
    :: GetConstraint t m b b
    => T.Text -- ^ the scoring step
    -> T.Text -- ^ the path fragment after the task part of the path
    -> IxTask -- ^ the task
    -> Event t a
    -> m (Event t b)
getIxTask _ _ IxTaskNone _ = return never
getIxTask step path (IxTask ii) ev = do
    let u :: T.Text
        u =
            mapUri
            $ "/"
            <> step
            <> "/"
            <> (T.pack . show $ ii)
            <> "/"
            <> path

    rsp <- performRequestAsync . fmap (req u) $ Nothing <$ ev
    return $ fmapMaybe decodeXhrResponse rsp

type Get t m b =
    forall a. GetConstraint t m b b => Event t a -> m (Event t b)

type GetIxTask' t m b = GetIxTask t m b b

type GetIxTask t m b c =
    forall a. GetConstraint t m b c => IxTask -> Event t a -> m (Event t c)

getTasks :: Get t m [Task]
getTasks = get "/comp-input/tasks"

getTaskLengths :: Get t m [TaskDistance]
getTaskLengths = get "/task-length/task-lengths"

getComps :: Get t m Comp
getComps = get "/comp-input/comps"

getNominals :: Get t m Nominal
getNominals = get "/comp-input/nominals"

getPilots :: Get t m [Pilot]
getPilots = get "/comp-input/pilots"

getPilotsStatus :: Get t m [(Pilot, [PilotTaskStatus])]
getPilotsStatus = get "/gap-point/pilots-status"

getValidity :: Get t m [Maybe Validity]
getValidity = get "/gap-point/validity"

getAllocation :: Get t m [Maybe Allocation]
getAllocation = get "/gap-point/allocation"

getTaskScore :: GetIxTask' t m [(Pilot, Breakdown)]
getTaskScore = getIxTask "gap-point" "score"

getTaskArrival :: GetIxTask' t m [(Pilot, TrackArrival)]
getTaskArrival = getIxTask "mask-track" "arrival"

getTaskLead :: GetIxTask' t m [(Pilot, TrackLead)]
getTaskLead = getIxTask "mask-track" "lead"

getTaskValidityWorking :: GetIxTask' t m (Maybe ValidityWorking)
getTaskValidityWorking = getIxTask "gap-point" "validity-working"

getTaskLength_ :: T.Text -> IxTask -> Get t m b
getTaskLength_ = getIxTask "task-length"

getTaskLengthSphericalEdge, getTaskLengthEllipsoidEdge
    :: GetIxTask' t m (OptimalRoute (Maybe TrackLine))
getTaskLengthSphericalEdge = getTaskLength_ "spherical-edge"
getTaskLengthEllipsoidEdge = getTaskLength_ "ellipsoid-edge"

getTaskLengthProjectedEdgeSpherical, getTaskLengthProjectedEdgeEllipsoid
    :: GetIxTask' t m (Maybe TrackLine)
getTaskLengthProjectedEdgeSpherical = getTaskLength_ "projected-edge-spherical"
getTaskLengthProjectedEdgeEllipsoid = getTaskLength_ "projected-edge-ellipsoid"

getTaskLengthProjectedEdgePlanar
    :: GetIxTask' t m (Maybe PlanarTrackLine)
getTaskLengthProjectedEdgePlanar = getTaskLength_ "projected-edge-planar"

getTaskPilotDf, getTaskPilotAbs :: GetIxTask' t m [Pilot]
getTaskPilotDf = getIxTask "gap-point" "pilot-df"
getTaskPilotAbs = getIxTask "comp-input" "pilot-abs"

getTaskPilotDnf :: GetIxTask t m [Pilot] Dnf
getTaskPilotDnf ix ev = do
    e <- getIxTask "comp-input" "pilot-dnf" ix ev
    return $ Dnf <$> e

getTaskPilotDfNoTrack :: GetIxTask t m [Pilot] DfNoTrack
getTaskPilotDfNoTrack ix ev = do
    e <- getIxTask "comp-input" "pilot-dfnt" ix ev
    return $ DfNoTrack <$> e

getTaskPilotNyp :: GetIxTask t m [Pilot] Nyp
getTaskPilotNyp ix ev = do
    e <- getIxTask "gap-point" "pilot-nyp" ix ev
    return $ Nyp <$> e

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

getTaskPilotTrackFlyingSection
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
getTaskPilotTrackFlyingSection IxTaskNone _ = return never
getTaskPilotTrackFlyingSection (IxTask ii) ev = do
    let u :: PilotId -> T.Text
        u (PilotId pid) =
            mapUri
            $ "/cross-zone/track-flying-section/"
            <> (T.pack . show $ ii)
            <> "/"
            <> (T.pack pid)

    let req' md = XhrRequest "GET" (u md) def
    rsp <- performRequestAsync . fmap req' $ getPilotId <$> ev
    return $ fmapMaybe decodeXhrResponse rsp

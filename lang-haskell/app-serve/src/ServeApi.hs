module ServeApi where

import Data.Time.Clock (UTCTime)
import Data.UnitsOfMeasure (u)
import Servant ((:<|>)(..), Capture, Get, JSON, Proxy(..), (:>))

import Flight.Units ()
import Flight.Clip (FlyingSection)
import Flight.Track.Cross (TrackFlyingSection(..), ZoneTag(..), TrackCross(..))
import Flight.Track.Stop (TrackScoredSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land (TaskLanding(..), TrackEffort(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified Flight.Track.Point as Alt (AltBreakdown(..))
import Flight.Track.Point (Allocation(..), Breakdown(..))
import qualified "flight-gap-valid" Flight.Score as Vy (Validity(..))
import qualified "flight-gap-valid" Flight.Score as Vw (ValidityWorking(..))
import "flight-gap-lead" Flight.Score (LeadingArea2Units)
import Flight.Comp
    ( Comp(..)
    , Task(..)
    , Nominal(..)
    , Pilot(..)
    , PilotTaskStatus(..)
    , DfNoTrackPilot(..)
    , Pilot(..)
    )
import Flight.Route (OptimalRoute(..), TrackLine(..) , PlanarTrackLine(..))
import Flight.Distance (QTaskDistance)
import ServeTrack (RawLatLngTrack(..), BolsterStats(..))
import ServeArea (RawLeadingArea(..))

type CompInputApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

type TaskLengthApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

    :<|> "task-length" :> Capture "task" Int :> "spherical-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "ellipsoid-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-spherical"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-ellipsoid"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-planar"
        :> Get '[JSON] PlanarTrackLine

    :<|> "task-length" :> "task-lengths"
        :> Get '[JSON] [QTaskDistance Double [u| m |]]

type GapPointApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

    :<|> "task-length" :> Capture "task" Int :> "spherical-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "ellipsoid-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-spherical"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-ellipsoid"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-planar"
        :> Get '[JSON] PlanarTrackLine

    :<|> "task-length" :> "task-lengths"
        :> Get '[JSON] [QTaskDistance Double [u| m |]]

    :<|> "stats" :> "point-diff"
        :> Get '[JSON] [Maybe (Double, Double)]

    :<|> "gap-point" :> "pilots-status"
        :> Get '[JSON] [(Pilot, [PilotTaskStatus])]
    :<|> "gap-point" :> "validity"
        :> Get '[JSON] [Maybe Vy.Validity]
    :<|> "gap-point" :> "allocation"
        :> Get '[JSON] [Maybe Allocation]
    :<|> "gap-point" :> Capture "task" Int :> "score"
        :> Get '[JSON] [(Pilot, Breakdown)]
    :<|> "gap-point" :> Capture "task" Int :> "validity-working"
        :> Get '[JSON] (Maybe Vw.ValidityWorking)

    :<|> "comp-input" :> Capture "task" Int :> "pilot-abs"
        :> Get '[JSON] [Pilot]
    :<|> "comp-input" :> Capture "task" Int :> "pilot-dnf"
        :> Get '[JSON] [Pilot]
    :<|> "comp-input" :> Capture "task" Int :> "pilot-dfnt"
        :> Get '[JSON] [DfNoTrackPilot]
    :<|> "gap-point" :> Capture "task" Int :> "pilot-nyp"
        :> Get '[JSON] [Pilot]
    :<|> "gap-point" :> Capture "task" Int :> "pilot-df"
        :> Get '[JSON] [Pilot]

    :<|> "pilot-track" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] RawLatLngTrack

    :<|> "discard-further" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] RawLeadingArea

    :<|> "cross-zone" :> "track-flying-section" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] TrackFlyingSection

    :<|> "peg-frame" :> "track-scored-section" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] TrackScoredSection

    :<|> "cross-zone" :> (Capture "task" Int) :> "flying-times"
        :> Get '[JSON] [(Pilot, FlyingSection UTCTime)]

    :<|> "cross-zone" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] (Maybe TrackCross)

    :<|> "tag-zone" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] [Maybe ZoneTag]

    :<|> "mask-track" :> (Capture "task" Int) :> "bolster-stats"
        :> Get '[JSON] BolsterStats

    :<|> "mask-track" :> (Capture "task" Int) :> "bonus-bolster-stats"
        :> Get '[JSON] BolsterStats

    :<|> "mask-track" :> (Capture "task" Int) :> "reach"
        :> Get '[JSON] [(Pilot, TrackReach)]

    :<|> "mask-track" :> (Capture "task" Int) :> "bonus-reach"
        :> Get '[JSON] [(Pilot, TrackReach)]

    :<|> "mask-track" :> (Capture "task" Int) :> "arrival"
        :> Get '[JSON] [(Pilot, TrackArrival)]

    :<|> "mask-track" :> (Capture "task" Int) :> "lead"
        :> Get '[JSON] [(Pilot, TrackLead LeadingArea2Units)]

    :<|> "mask-track" :> (Capture "task" Int) :> "time"
        :> Get '[JSON] [(Pilot, TrackSpeed)]

    :<|> "land-out" :> (Capture "task" Int) :> "effort"
        :> Get '[JSON] [(Pilot, TrackEffort)]

    :<|> "land-out" :> (Capture "task" Int) :> "landing"
        :> Get '[JSON] (Maybe TaskLanding)

    :<|> "fs-effort" :> (Capture "task" Int) :> "landing"
        :> Get '[JSON] (Maybe TaskLanding)

    :<|> "fs-route" :> Capture "task" Int :> "sphere"
        :> Get '[JSON] (Maybe TrackLine)

    :<|> "fs-route" :> Capture "task" Int :> "ellipse"
        :> Get '[JSON] (Maybe TrackLine)

    :<|> "fs-score" :> "validity"
        :> Get '[JSON] [Maybe Vy.Validity]

    :<|> "fs-score" :> Capture "task" Int :> "validity-working"
        :> Get '[JSON] (Maybe Vw.ValidityWorking)

    :<|> "fs-score" :> (Capture "task" Int) :> "score"
        :> Get '[JSON] [(Pilot, Alt.AltBreakdown)]

    :<|> "fs-mask-track" :> (Capture "task" Int) :> "arrival"
        :> Get '[JSON] [(Pilot, TrackArrival)]

compInputApi :: Proxy (CompInputApi k)
compInputApi = Proxy

taskLengthApi :: Proxy (TaskLengthApi k)
taskLengthApi = Proxy

gapPointApi :: Proxy (GapPointApi k)
gapPointApi = Proxy

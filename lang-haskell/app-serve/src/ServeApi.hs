module ServeApi where

import Prelude hiding (abs)
import GHC.Records
import qualified Data.Text as T (Text)
import Data.Time.Clock (UTCTime)
import System.Environment (getProgName)
import Text.Printf (printf)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (isNothing, catMaybes)
import Data.List ((\\), nub, sort, sortOn, find, zip4)
import qualified Data.Map.Strict as Map
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Servant
    ( (:<|>)(..)
    , Capture, Get, JSON, Server, Handler(..), Proxy(..), ServantErr
    , (:>)
    , errBody, err400, hoistServer, serve, throwError
    )
import Data.Swagger (Swagger(..), URL(..), url, info, title, version, license, description)
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Lens hiding (ix)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Numeric.Sampling
import System.FilePath (takeFileName)

import Flight.Units ()
import Flight.Clip (FlyingSection)
import qualified Flight.Track.Cross as Cg (Crossing(..), PilotTrackCross(..))
import qualified Flight.Track.Tag as Tg (Tagging(..), PilotTrackTag(..))
import qualified Flight.Track.Stop as Sp (Framing(..))
import Flight.Track.Cross (TrackFlyingSection(..), ZoneTag(..), TrackCross(..))
import Flight.Track.Stop (TrackScoredSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land
    (Landing(..), TaskLanding(..), TrackEffort(..), effortRank, taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (DiscardingLead(..), TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Time (TickRow(..))
import qualified Flight.Track.Mask as Mask (MaskingReach(..), MaskingArrival(..))
import Flight.Track.Mask
    ( MaskingEffort(..)
    , MaskingLead(..)
    , MaskingReach(..)
    , MaskingSpeed(..)
    )
import qualified Flight.Track.Point as Norm (NormPointing(..), NormBreakdown(..))
import Flight.Track.Point
    (Pointing(..), Velocity(..), Allocation(..), Breakdown(..))
import qualified "flight-gap-weight" Flight.Score as Wg (Weights(..))
import qualified "flight-gap-valid" Flight.Score as Vy (Validity(..))
import qualified "flight-gap-valid" Flight.Score as Vw (ValidityWorking(..))
import "flight-gap-allot" Flight.Score (PilotVelocity(..))
import "flight-gap-lead" Flight.Score (LeadingArea2Units)
import "flight-gap-math" Flight.Score (TaskPoints(..))
import "flight-gap-valid" Flight.Score
    ( DistanceValidity(..), LaunchValidity(..), TimeValidity(..)
    , TaskValidity(..), StopValidity(..)
    )
import "flight-gap-weight" Flight.Score
    ( DistanceWeight(..), ReachWeight(..), EffortWeight(..)
    , LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    )
import Flight.Scribe
    ( readComp, readNormArrival, readNormLandout, readNormRoute, readNormScore
    , readRoute, readCrossing, readTagging, readFraming
    , readMaskingArrival
    , readMaskingEffort
    , readDiscardingLead
    , readMaskingLead
    , readMaskingReach
    , readMaskingSpeed
    , readBonusReach
    , readLanding, readPointing
    , readPilotDiscardFurther
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , Comp(..)
    , Task(..)
    , Nominal(..)
    , PilotTrackLogFile(..)
    , IxTask(..)
    , PilotId(..)
    , Pilot(..)
    , PilotTaskStatus(..)
    , PilotGroup(..)
    , DfNoTrack(..)
    , DfNoTrackPilot(..)
    , Nyp(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , LeadAreaFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , GapPointFile(..)
    , NormArrivalFile(..)
    , NormLandoutFile(..)
    , NormRouteFile(..)
    , NormScoreFile(..)
    , Pilot(..)
    , findCompInput
    , compToNormArrival
    , compToNormLandout
    , compToNormRoute
    , compToNormScore
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , compToMaskEffort
    , compToLeadArea
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    , compToLand
    , compToPoint
    , crossToTag
    , tagToPeg
    , ensureExt
    )
import Flight.Route
    ( OptimalRoute(..), TaskTrack(..), TrackLine(..), GeoLines(..)
    , ProjectedTrackLine(..), PlanarTrackLine(..)
    )
import Flight.Mask (checkTracks)
import Data.Ratio.Rounding (dpRound)
import Flight.Distance (QTaskDistance)
import qualified ServeOptions as Opt (description)
import ServeTrack (RawLatLngTrack(..), BolsterStats(..), crossToTrack, tagToTrack)
import ServeArea (RawLeadingArea(..))
import ServeValidity (nullValidityWorking)
import ServeSwagger (SwagUiApi)

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
        :> Get '[JSON] [(Pilot, Norm.NormBreakdown)]

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

    :<|> "fs-mask-track" :> (Capture "task" Int) :> "arrival"
        :> Get '[JSON] [(Pilot, TrackArrival)]

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

compInputApi :: Proxy (CompInputApi k)
compInputApi = Proxy

taskLengthApi :: Proxy (TaskLengthApi k)
taskLengthApi = Proxy

gapPointApi :: Proxy (GapPointApi k)
gapPointApi = Proxy

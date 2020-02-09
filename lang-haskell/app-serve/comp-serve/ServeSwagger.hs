{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoPatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServeSwagger (SwagUiApi, BolsterStats(..)) where

import Data.Ratio
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))
import Servant (Proxy(..))
import Data.Swagger
import Servant.Swagger.UI
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showUnit)

import Flight.Zone.ZoneKind
import Flight.Zone.MkZones (Discipline(..), Zones(..))
import Flight.Zone.TaskZones (TaskZones(..))
import Flight.Zone.Raw
import Flight.Zone
import Flight.Distance
import Flight.LatLng
import Flight.LatLng.Raw
import Flight.Geodesy
import Flight.Earth.Ellipsoid
import Flight.Route
import Flight.Score
import Flight.Comp
import Flight.Track.Arrival
import Flight.Track.Land
import Flight.Track.Distance
import Flight.Track.Speed
import Flight.Track.Time
import Flight.Track.Lead
import Flight.Track.Point
import Flight.Track.Cross
import Flight.Track.Stop
import Flight.Gap.Fraction (Fractions)
import Flight.EastNorth
import qualified Flight.Kml as Kml
import ServeTrack (RawLatLngTrack(..))

data BolsterStats =
    BolsterStats
        { bolster :: ReachStats
        , reach :: ReachStats
        }
    deriving (Generic, ToJSON)

type SwagUiApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

deriving instance Generic LwScaling
deriving instance Generic (Radius q)
deriving instance Generic (Alt q)
deriving instance Generic RawLat
deriving instance Generic RawLng
deriving instance Generic RawLatLng
deriving instance Generic (ScoreBackTime k)
deriving instance Generic EarthMath
deriving instance Generic Projection
deriving instance Generic (NominalDistance q)
deriving instance Generic (MinimumDistance q)
deriving instance Generic (NominalTime q)
deriving instance Generic NominalLaunch
deriving instance Generic NominalGoal
deriving instance Generic (SecondsPerPoint k)
deriving instance Generic TooEarlyPoints
deriving instance Generic (JumpTheGunLimit k)
deriving instance Generic (TaskDistance q)
deriving instance Generic (Chunk q)
deriving instance Generic (FlownMax q)
deriving instance Generic (PilotDistance q)
deriving instance Generic RelativeDifficulty
deriving instance Generic DifficultyFraction
deriving instance Generic TaskValidity
deriving instance Generic LaunchValidity
deriving instance Generic DistanceValidity
deriving instance Generic TimeValidity
deriving instance Generic StopValidity
deriving instance Generic PilotsFlying
deriving instance Generic PilotsPresent
deriving instance Generic (SumOfDistance q)
deriving instance Generic NominalDistanceArea
deriving instance Generic (BestTime q)
deriving instance Generic (LaunchToEss q)
deriving instance Generic (FlownMean q)
deriving instance Generic (FlownStdDev q)
deriving instance Generic PilotsLanded
deriving instance Generic (PilotTime q)
deriving instance Generic (LeadingArea q)
deriving instance Generic (LeadingCoef q)
deriving instance Generic TaskPlacing
deriving instance Generic TaskPoints
deriving instance Generic LinearPoints
deriving instance Generic DifficultyPoints
deriving instance Generic DistancePoints
deriving instance Generic LeadingPoints
deriving instance Generic ArrivalPoints
deriving instance Generic TimePoints
deriving instance Generic LinearFraction
deriving instance Generic DistanceFraction
deriving instance Generic LeadingFraction
deriving instance Generic ArrivalFraction
deriving instance Generic SpeedFraction
deriving instance Generic GoalRatio
deriving instance Generic ReachWeight
deriving instance Generic EffortWeight
deriving instance Generic DistanceWeight
deriving instance Generic LeadingWeight
deriving instance Generic ArrivalWeight
deriving instance Generic TimeWeight
deriving instance Generic (JumpedTheGun q)
deriving instance Generic (PilotVelocity q)
deriving instance Generic RawLatLngTrack
deriving instance Generic RawAlt
deriving instance Generic (ArrivalLag q)
deriving instance Generic ArrivalPlacing

instance ToSchema (Ratio Integer) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance (KnownUnit (Unpack u)) => ToSchema (Quantity Double u) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy String)

instance ToSchema (TaskZones OpenDistance Double) where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
        return . NamedSchema (Just "OpenDistance") $ mempty
          & (type_ .~ SwaggerObject)
          & properties .~
              [ ("prolog", doubleSchema)
              , ("open-mandatory", doubleSchema)
              , ("open-free", doubleSchema)
              ]
          & required .~ ["prolog", "open-mandatory", "open-free"]

instance ToSchema (TaskZones Race Double) where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
        return . NamedSchema (Just "Race") $ mempty
          & (type_ .~ SwaggerObject)
          & properties .~
              [ ("prolog", doubleSchema)
              , ("race", doubleSchema)

              -- TzEssIsNotGoal fields
              , ("race-ess", doubleSchema)
              , ("epilog", doubleSchema)
              , ("goal", doubleSchema)

              -- TzEssIsGoal fields
              , ("race-ess-is-goal", doubleSchema)
              ]
          & required .~ ["prolog", "race"]

dpUnit :: Int -> String -> ParamSchema t
dpUnit n unit = mempty
    & type_ .~ SwaggerString
    & pattern .~ (Just (T.pack ("^" ++ [r|\-?\d+\.\d{0,|] ++ show n ++ "} " ++ unit ++ "$")))

instance ToSchema q => ToSchema (NominalDistance q) where
    declareNamedSchema _ =
        pure . NamedSchema Nothing $ mempty
              & example ?~ "5.0 km"
              & paramSchema .~ (dpUnit 1 (showUnit (undefined :: proxy [u| km |])))

instance ToSchema (Zones)
instance ToSchema (RawZone)
instance ToSchema q => ToSchema (Alt q)
instance ToSchema (RawLat)
instance ToSchema (RawLng)
instance ToSchema (RawLatLng)
instance ToSchema (EarthMath)
instance ToSchema (Projection)
instance ToSchema (Ellipsoid Double)
instance ToSchema (EarthModel Double)
instance ToSchema (Discipline)
instance ToSchema (UtcOffset)
instance ToSchema (Give)
instance ToSchema (LwScaling)
instance ToSchema k => ToSchema (JumpTheGunLimit k)
instance ToSchema (PointPenalty)
instance ToSchema (TooEarlyPoints)
instance ToSchema (EarlyStart)
instance ToSchema (OpenClose)
instance ToSchema (StartGate)
instance ToSchema (TaskStop)
instance ToSchema k => ToSchema (ScoreBackTime k)
instance ToSchema (Tweak)
instance ToSchema (PilotId)
instance ToSchema (PilotName)
instance ToSchema (Pilot)
instance ToSchema q => ToSchema (Radius q)
instance ToSchema q => ToSchema (SecondsPerPoint q)
instance ToSchema (Task k)
instance ToSchema (Comp)
instance ToSchema (Nominal)
instance ToSchema q => ToSchema (MinimumDistance q)
instance ToSchema q => ToSchema (NominalTime q)
instance ToSchema q => ToSchema (TaskDistance q)
instance ToSchema (NominalLaunch)
instance ToSchema (NominalGoal)
instance ToSchema TrackLine
instance ToSchema TaskLanding
instance ToSchema Lookahead
instance ToSchema Chunking
instance ToSchema q => ToSchema (Chunk q)
instance ToSchema SumOfDifficulty
instance ToSchema ChunkDifficulty
instance ToSchema RelativeDifficulty
instance ToSchema DifficultyFraction
instance ToSchema IxChunk
instance ToSchema q => ToSchema (PilotDistance q)
instance ToSchema q => ToSchema (FlownMax q)
instance ToSchema (OptimalRoute (Maybe TrackLine))
instance ToSchema Validity
instance ToSchema TaskValidity
instance ToSchema LaunchValidity
instance ToSchema DistanceValidity
instance ToSchema TimeValidity
instance ToSchema StopValidity
instance ToSchema ValidityWorking
instance ToSchema LaunchValidityWorking
instance ToSchema PilotsFlying
instance ToSchema PilotsPresent
instance ToSchema DistanceValidityWorking
instance ToSchema q => ToSchema (SumOfDistance q)
instance ToSchema q => ToSchema (ReachToggle (FlownMax q))
instance ToSchema NominalDistanceArea
instance ToSchema TimeValidityWorking
instance ToSchema q => ToSchema (BestTime q)
instance ToSchema StopValidityWorking
instance ToSchema q => ToSchema (LaunchToEss q)
instance ToSchema ReachStats
instance ToSchema (ReachToggle ReachStats)
instance ToSchema q => ToSchema (FlownMean q)
instance ToSchema q => ToSchema (FlownStdDev q)
instance ToSchema PilotsAtEss
instance ToSchema PilotsLanded
instance ToSchema NormBreakdown
instance ToSchema q => ToSchema (ReachToggle (TaskDistance q))
instance ToSchema q => ToSchema (PilotTime q)
instance ToSchema q => ToSchema (LeadingArea q)
instance ToSchema q => ToSchema (LeadingCoef q)
instance ToSchema TaskPlacing
instance ToSchema TaskPoints
instance ToSchema Points
instance ToSchema LinearPoints
instance ToSchema DifficultyPoints
instance ToSchema DistancePoints
instance ToSchema LeadingPoints
instance ToSchema ArrivalPoints
instance ToSchema TimePoints
instance ToSchema Fractions
instance ToSchema LinearFraction
instance ToSchema DistanceFraction
instance ToSchema LeadingFraction
instance ToSchema ArrivalFraction
instance ToSchema SpeedFraction
instance ToSchema PlanarTrackLine
instance ToSchema UtmZone
instance ToSchema EastingNorthing
instance ToSchema PilotTaskStatus
instance ToSchema Allocation
instance ToSchema GoalRatio
instance ToSchema Weights
instance ToSchema ReachWeight
instance ToSchema EffortWeight
instance ToSchema DistanceWeight
instance ToSchema LeadingWeight
instance ToSchema ArrivalWeight
instance ToSchema TimeWeight
instance ToSchema Breakdown
instance ToSchema q => ToSchema (JumpedTheGun q)
instance ToSchema q => ToSchema (ReachToggle (PilotDistance q))
instance ToSchema Velocity
instance ToSchema q => ToSchema (PilotVelocity q)
instance ToSchema DfNoTrackPilot
instance ToSchema AwardedDistance
instance ToSchema (ReachToggle AwardedDistance)
instance ToSchema AwardedVelocity
instance ToSchema RawLatLngTrack
instance ToSchema Kml.MarkedFixes
instance ToSchema Kml.Fix
instance ToSchema Kml.Seconds
instance ToSchema Kml.LLA
instance ToSchema Kml.Latitude
instance ToSchema Kml.Longitude
instance ToSchema Kml.Altitude
instance ToSchema Seconds
instance ToSchema TrackFlyingSection
instance ToSchema TrackScoredSection
instance ToSchema ZoneTag
instance ToSchema InterpolatedFix
instance ToSchema RawAlt
instance ToSchema ZoneCross
instance ToSchema Fix
instance ToSchema BolsterStats
instance ToSchema TrackReach
instance ToSchema TrackArrival
instance ToSchema q => ToSchema (ArrivalLag q)
instance ToSchema ArrivalPlacing
instance ToSchema TrackLead
instance ToSchema TrackSpeed
instance ToSchema TrackEffort

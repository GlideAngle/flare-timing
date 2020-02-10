{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoPatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServeSwagger (SwagUiApi) where

import Data.Ratio
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Lens
import Servant (Proxy(..))
import Data.Swagger
import Servant.Swagger.UI
import Data.UnitsOfMeasure (KnownUnit, Unpack)
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
import ServeTrack (RawLatLngTrack(..), BolsterStats(..))

type SwagUiApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

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
    -- SEE: https://www.regular-expressions.info/floatingpoint.html
    & pattern .~ (Just (T.pack ([r|^[-+]?[0-9]*\.?[0-9]{0,|] ++ show n ++ "} " ++ unit ++ "$")))

instance (KnownUnit (Unpack u), q ~ Quantity a u, ToSchema q) => ToSchema (NominalDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ (dpUnit 1 (showUnit (undefined :: proxy u)))
        & example ?~ "5 km"

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
instance ToSchema TrackReach
instance ToSchema TrackArrival
instance ToSchema q => ToSchema (ArrivalLag q)
instance ToSchema ArrivalPlacing
instance ToSchema TrackLead
instance ToSchema TrackSpeed
instance ToSchema TrackEffort

instance ToSchema RawLatLngTrack
instance ToSchema BolsterStats

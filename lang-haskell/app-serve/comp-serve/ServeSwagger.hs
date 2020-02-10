{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoPatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServeSwagger (SwagUiApi) where

import Data.Ratio
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Lens
import Data.Aeson (ToJSON(..))
import Servant (Proxy(..))
import Data.Swagger
import Servant.Swagger.UI
import Data.UnitsOfMeasure (KnownUnit, Unpack)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showUnit)

import Data.Via.Scientific (DecimalPlaces(..))
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

_dpNum :: DecimalPlaces -> ParamSchema t
_dpNum (DecimalPlaces n) = mempty
    & type_ .~ SwaggerString
    -- SEE: https://www.regular-expressions.info/floatingpoint.html
    & pattern .~ Just (T.pack ([r|^[-+]?[0-9]*\.?[0-9]{0,|] ++ show n ++ "}$"))

dpUnit :: DecimalPlaces -> String -> ParamSchema t
dpUnit (DecimalPlaces n) unit
    | n <= 0 = intUnit unit
    | otherwise = mempty
        & type_ .~ SwaggerString
        -- SEE: https://www.regular-expressions.info/floatingpoint.html
        & pattern .~ Just (T.pack ([r|^[-+]?[0-9]*\.?[0-9]{0,|] ++ show n ++ "} " ++ unit ++ "$"))

intUnit :: String -> ParamSchema t
intUnit unit = mempty
    & type_ .~ SwaggerString
    -- SEE: https://www.regular-expressions.info/floatingpoint.html
    & pattern .~ Just (T.pack ([r|^[-+]?[0-9]+ |] ++ unit ++ "$"))

posUnit :: String -> ParamSchema t
posUnit unit = mempty
    & type_ .~ SwaggerString
    -- SEE: https://www.regular-expressions.info/floatingpoint.html
    & pattern .~ Just (T.pack ([r|^[+]?[0-9]+ |] ++ unit ++ "$"))

instance (KnownUnit (Unpack u), q ~ Quantity a u, ToSchema q) => ToSchema (NominalDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 1) (showUnit (undefined :: proxy u))
        & example ?~ "120.0 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (Alt q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "771.000 m"

instance ToSchema RawLat where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy Double)
        & minimum_ ?~ -90
        & maximum_ ?~ 90
        & example ?~ "32.2176"

instance ToSchema RawLng where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy Double)
        & minimum_ ?~ -180
        & maximum_ ?~ 180
        & example ?~ "-101.71603"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (Radius q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 1) (showUnit (undefined :: proxy u))
        & example ?~ "400.0 m"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (JumpTheGunLimit q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ posUnit (showUnit (undefined :: proxy u))
        & example ?~ "300 s"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (SecondsPerPoint q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ posUnit (showUnit (undefined :: proxy u))
        & example ?~ "3 s"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (MinimumDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "5.000 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (NominalTime q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "2.000000 h"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (ScoreBackTime q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "900.000 s"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (FlownMax q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 6) (showUnit (undefined :: proxy u))
        & example ?~ "126.747570 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (FlownMean q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 6) (showUnit (undefined :: proxy u))
        & example ?~ "106.814413 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (FlownStdDev q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 6) (showUnit (undefined :: proxy u))
        & example ?~ "37.629588 km"

instance ToSchema PilotId where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy String)
        & example ?~ toJSON ("101" :: String)

instance ToSchema PilotName where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy String)
        & example ?~ toJSON ("Davis Straub" :: String)

instance ToSchema Pilot where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy [String])
        & example ?~ toJSON (["101", "Davis Straub"] :: [String])

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (PilotDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "62.255 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (Chunk q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 1) (showUnit (undefined :: proxy u))
        & example ?~ "62.2 km"

instance {-# OVERLAPPING #-} ToSchema (Chunk q) => ToSchema (IxChunk, Chunk q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~ toJSON (["1171", "117.1 km"] :: [String])

instance ToSchema Zones
instance ToSchema RawZone
instance ToSchema RawLatLng
instance ToSchema EarthMath
instance ToSchema Projection
instance ToSchema (Ellipsoid Double)
instance ToSchema (EarthModel Double)
instance ToSchema Discipline
instance ToSchema UtcOffset
instance ToSchema Give
instance ToSchema LwScaling
instance ToSchema PointPenalty
instance ToSchema TooEarlyPoints
instance ToSchema EarlyStart
instance ToSchema OpenClose
instance ToSchema StartGate
instance ToSchema TaskStop
instance ToSchema Tweak
instance ToSchema (Task k)
instance ToSchema Comp
instance ToSchema Nominal
instance ToSchema q => ToSchema (TaskDistance q)
instance ToSchema NominalLaunch
instance ToSchema NominalGoal
instance ToSchema TrackLine
instance ToSchema TaskLanding
instance ToSchema Lookahead
instance ToSchema Chunking
instance ToSchema SumOfDifficulty
instance ToSchema ChunkDifficulty
instance ToSchema RelativeDifficulty
instance ToSchema DifficultyFraction
instance ToSchema IxChunk
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
instance ToSchema (FlownMax q) => ToSchema (ReachToggle (FlownMax q))
instance ToSchema NominalDistanceArea
instance ToSchema TimeValidityWorking
instance ToSchema q => ToSchema (BestTime q)
instance ToSchema StopValidityWorking
instance ToSchema q => ToSchema (LaunchToEss q)
instance ToSchema ReachStats
instance ToSchema (ReachToggle ReachStats)
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
instance ToSchema (PilotDistance q) => ToSchema (ReachToggle (PilotDistance q))
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

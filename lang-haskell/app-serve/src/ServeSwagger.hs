{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoPatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServeSwagger (SwagUiApi) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Ratio
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Lens
import Data.Aeson (ToJSON(..))
import Servant (Proxy(..))
import Data.Swagger
import Servant.Swagger.UI
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showUnit)

import Data.Via.Scientific (DecimalPlaces(..))
import Flight.Clip (FlyingSection)
import Flight.Track.Mask
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
import "flight-gap-allot" Flight.Score
import "flight-gap-effort" Flight.Score
import "flight-gap-lead" Flight.Score
import "flight-gap-math" Flight.Score
import "flight-gap-stop" Flight.Score (ScoreBackTime(..))
import "flight-gap-valid" Flight.Score
import "flight-gap-weight" Flight.Score
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
import Flight.EastNorth
import qualified Flight.Kml as Kml
import ServeTrack (RawLatLngTrack(..), BolsterStats(..))
import ServeArea (RawLeadingArea(..))

type SwagUiApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

instance ToSchema DifficultyFraction where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema (PointPenalty Mul) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema (PointPenalty Add) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance ToSchema (PointPenalty Reset) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy (Maybe Double))

instance ToSchema TooEarlyPoints where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Int)

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

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (BestTime q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 6) (showUnit (undefined :: proxy u))
        & example ?~ "3.718498 h"

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

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (SumOfDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 1) (showUnit (undefined :: proxy u))
        & example ?~ "9487.1 km"

instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToSchema q) => ToSchema (LaunchToEss q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 3) (showUnit (undefined :: proxy u))
        & example ?~ "142.789 km"

instance ToSchema PilotId where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy String)
        & example ?~ toJSON (PilotId "101")

instance ToSchema PilotName where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy String)
        & example ?~ toJSON (PilotName "Davis Straub")

instance ToSchema Pilot where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy [String])
        & example ?~ toJSON (Pilot (PilotId "101", PilotName "Davis Straub"))

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

instance {-# OVERLAPPING #-} ToSchema (Pilot, TrackEffort) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "106", PilotName "Larry Bunner")
                 , TrackEffort
                      { frac = DifficultyFraction 0.32864056
                      , effort = TaskDistance $ convert [u| 24.828000 km |]
                      }
                 )

instance {-# OVERLAPPING #-} ToSchema (Pilot, TrackSpeed) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "101", PilotName "Davis Straub")
                 , TrackSpeed
                      { frac = SpeedFraction 0.68107665
                      , time = PilotTime [u| 2.824138 h |]
                      }
                 )

instance {-# OVERLAPPING #-} (u ~ LeadingArea2Units) => ToSchema (Pilot, TrackLead u) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "105", PilotName "Cory Barnwell")
                 , TrackLead
                      { frac = LeadingFraction 0.80786961
                      , area = LeadingArea [u| 42221610.0724 km^2 s |]
                      , coef = LeadingCoef 1.70921115
                      }
                 )

instance {-# OVERLAPPING #-} ToSchema (Pilot, TrackArrival) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "33", PilotName "Niki Longshore")
                 , TrackArrival
                      { frac = ArrivalFraction 0.54348856
                      , rank = ArrivalPlacing 8
                      , lag = ArrivalLag [u| 0.215871 h |]
                      }
                 )

instance {-# OVERLAPPING #-} ToSchema (Pilot, TrackReach) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "42", PilotName "Mikhail Karmazin")
                 , TrackReach
                      { frac = LinearFraction 1
                      , reach = TaskDistance $ convert [u| 111.259762 km |]
                      }
                 )

instance {-# OVERLAPPING #-} ToSchema (Pilot, FlyingSection UTCTime) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                ( Pilot (PilotId "36", PilotName "Brad Porter")
                , (read "2017-04-12 01:55:24 UTC", read "2017-04-12 05:53:01 UTC") :: (UTCTime, UTCTime)
                )

instance ToSchema TrackScoredSection where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            (toJSON $
                let (t0, t1) = (read "2017-04-12 01:55:24 UTC", read "2017-04-12 05:53:01 UTC") in
                TrackScoredSection
                    { scoredFixes = Just (0, 7672)
                    , scoredSeconds = Just (0, 14257)
                    , scoredTimes = Just (t0, t1)
                    , scoredWindowSeconds = Just . Seconds . round $ t1 `diffUTCTime` t0
                    })

instance ToSchema TrackFlyingSection where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                TrackFlyingSection
                    { flyingSeconds = Just (0, 14257)
                    , loggedSeconds = Just $ Seconds 14257
                    , loggedTimes = Just (read "2017-04-12 01:55:24 UTC", read "2017-04-12 05:53:01 UTC")
                    , flyingFixes = Just (0, 7672)
                    , flyingTimes = Just (read "2017-04-12 01:55:24 UTC", read "2017-04-12 05:53:01 UTC")
                    , loggedFixes = Just 7673
                    }

instance ToSchema AwardedDistance where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                AwardedDistance
                    { awardedFrac = 0.9948250462723964
                    , awardedMade = TaskDistance $ convert [u| 158.020000 km |]
                    , awardedTask = TaskDistance $ convert [u| 158.842000 km |]
                    }

instance {-# OVERLAPPING #-} ToSchema (Pilot, [PilotTaskStatus]) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                 ( Pilot (PilotId "4", PilotName "Frank Chetcuti")
                 , [DF, DFNoTrack, DF, DFNoTrack, DF, ABS, DF] :: [PilotTaskStatus]
                 )

instance {-# OVERLAPPING #-} ToSchema (Maybe (Double, Double)) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~ (toJSON $ ((Just (-16.82, 50.94298381524192)) :: Maybe (Double, Double)))
        & description ?~ "The difference in the mean of the points and the standard deviation in the points for a task."

instance (KnownUnit (Unpack u), q ~ Quantity a u, ToSchema q) => ToSchema (TaskDistance q) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & paramSchema .~ dpUnit (DecimalPlaces 6) (showUnit (undefined :: proxy u))
        & example ?~ "142.789048 km"

instance {-# OVERLAPPING #-} (KnownUnit (Unpack u), q ~ Quantity a u, ToSchema q) => ToSchema [TaskDistance q] where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                ((TaskDistance . convert <$>
                    [ [u| 142.789048 km |]
                    , [u| 185.745298 km |]
                    , [u| 195.224984 km |]
                    , [u| 388.750173 km |]
                    , [u| 153.936362 km |]
                    , [u| 166.415908 km |]
                    , [u| 172.107822 km |]
                    ]) :: [TaskDistance (Quantity Double [u| m |])])

instance ToSchema UtmZone where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                UtmZone
                    { lngZone = 55
                    , latZone = 'H'
                    }

instance {-# OVERLAPPING #-} ToSchema [(Pilot, PenaltySeq, String)] where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                ([
                    ( Pilot (PilotId "24", PilotName "Brodrick Osborne")
                    , addSeq 5.7
                    , "Jumped the gun by 0:57"
                    )
                ,
                    ( Pilot (PilotId "34", PilotName "Petr Polach")
                    , mulSeq 0
                    , "Airspace Infringement"
                    )
                ,
                    ( Pilot (PilotId "75", PilotName "Josh Woods")
                    , mulSeq 0.1
                    , "Altitude violation of 01:41 - 10%"
                    )
                ,
                    ( Pilot (PilotId "105", PilotName "Alan Arcos")
                    , mulSeq (negate 0.2)
                    , "Handicap"
                    )
                ] :: [(Pilot, PenaltySeq, String)])

instance {-# OVERLAPPING #-} ToSchema (Pilot, Breakdown) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
               ( Pilot (PilotId "5", PilotName "Jonny Durand")
               ,
                   Breakdown
                       { place = TaskPlacing 1
                       , subtotal = TaskPoints 1000
                       , demeritFrac = TaskPoints 0
                       , demeritPoint = TaskPoints 0
                       , demeritReset = TaskPoints 0
                       , total = TaskPoints 1000
                       , essNotGoal = Just $ EssNotGoal False
                       , penaltiesEssNotGoal = nullSeqs
                       , jump = Nothing
                       , penaltiesJumpRaw = Nothing
                       , penaltiesJumpEffective = nullSeqs
                       , penalties = nullSeqs
                       , penaltyReason = ""
                       , breakdown =
                           Points
                               { time = TimePoints 493.7
                               , reach = LinearPoints 217.88
                               , distance = DistancePoints 435.761
                               , leading = LeadingPoints 0
                               , effort = DifficultyPoints 217.88
                               , arrival = ArrivalPoints 70.5
                               }
                       , velocity =
                           Just
                               Velocity
                                   { gs = Just . StartGate $ read "2017-04-09 03:40:00 UTC"
                                   , ssVelocity = Just $ PilotVelocity [u| 40.44 km / h |]
                                   , ssElapsed = Just $ PilotTime [u| 2.462225 h |]
                                   , gsElapsed = Just $ PilotTime [u| 2.469229 h |]
                                   , es = Just $ read "2017-04-09 06:08:09.222861501556 UTC"
                                   , ss = Just $ read "2017-04-09 03:40:25.213746678338 UTC"
                                   , ssDistance = Just $ PilotDistance [u| 99.561 km |]
                                   , gsVelocity = Just $ PilotVelocity [u| 40.30 km / h |]
                                   }
                      , reach =
                          Just
                              ReachToggle
                                  { extra = PilotDistance [u| 99.561 km |]
                                  , flown = PilotDistance [u| 99.561 km |]
                                  }
                      , landedMade = Just $ PilotDistance [u| 99.561 km |]
                      , stoppedAlt = Nothing
                      }
                )

instance {-# OVERLAPPING #-} ToSchema (Pilot, NormBreakdown) where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
               ( Pilot (PilotId "5", PilotName "Jonny Durand")
               ,
                   NormBreakdown
                       { place = TaskPlacing 1
                       , total = TaskPoints 1000
                       , breakdown =
                           Points
                               { time = TimePoints 493.7
                               , reach = LinearPoints 217.88
                               , distance = DistancePoints 435.8
                               , leading = LeadingPoints 0
                               , effort = DifficultyPoints 217.88
                               , arrival = ArrivalPoints 70.5
                               }
                        , fractions =
                            Fractions
                                { time = SpeedFraction 1
                                , reach = LinearFraction 1
                                , distance = DistanceFraction 1
                                , leading = LeadingFraction 0
                                , effort = DifficultyFraction 1
                                , arrival = ArrivalFraction 1
                                }
                      , reach =
                          ReachToggle
                              { extra = TaskDistance $ convert [u| 99.531000 km |]
                              , flown = TaskDistance $ convert [u| 99.531000 km |]
                              }
                      , landedMade = TaskDistance $ convert [u| 99.530000 km |]
                      , ss = Just $ read "2017-04-09 03:40:00 UTC"
                      , es = Just $ read "2017-04-09 06:08:09 UTC"
                      , timeElapsed = Just $ PilotTime [u| 2.469167 h |]
                      , leadingArea = LeadingArea [u| 0 km^2 s |]
                      , leadingCoef = LeadingCoef 0
                      }
                )

instance ToSchema Allocation where
    declareNamedSchema _ = pure . NamedSchema Nothing $ mempty
        & example ?~
            toJSON
                Allocation
                    { goalRatio = GoalRatio 0.46808511
                    , points =
                        Points
                            { time = TimePoints 493.7
                            , reach = LinearPoints 217.88
                            , distance = DistancePoints 435.8
                            , leading = LeadingPoints 0
                            , effort = DifficultyPoints 217.88
                            , arrival = ArrivalPoints 70.5
                            }
                    , weight =
                        Weights
                            { time = TimeWeight 0.4937
                            , reach = ReachWeight 0.2179
                            , distance = DistanceWeight 0.4358
                            , leading = LeadingWeight 0
                            , effort = EffortWeight 0.2179
                            , arrival = ArrivalWeight 0.0705
                            }
                    , taskPoints = TaskPoints 1000
                    }

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
instance ToSchema PenaltySeq
instance ToSchema PenaltySeqs
instance ToSchema EarlyStart
instance ToSchema OpenClose
instance ToSchema StartGate
instance ToSchema TaskStop
instance ToSchema EGwScaling
instance ToSchema Tweak
instance ToSchema (Task k)
instance ToSchema Comp
instance ToSchema Nominal
instance ToSchema NominalLaunch
instance ToSchema NominalGoal
instance ToSchema TrackLine
instance ToSchema TaskLanding
instance ToSchema Lookahead
instance ToSchema Chunking
instance ToSchema SumOfDifficulty
instance ToSchema ChunkDifficulty
instance ToSchema RelativeDifficulty
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
instance ToSchema (FlownMax q) => ToSchema (ReachToggle (FlownMax q))
instance ToSchema NominalDistanceArea
instance ToSchema TimeValidityWorking
instance ToSchema StopValidityWorking
instance ToSchema ReachStats
instance ToSchema (ReachToggle (Maybe ReachStats))
instance ToSchema PilotsAtEss
instance ToSchema PilotsLanded
instance ToSchema NormBreakdown
instance ToSchema (TaskDistance q) => ToSchema (ReachToggle (TaskDistance q))
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
instance ToSchema EastingNorthing
instance ToSchema PilotTaskStatus
instance ToSchema GoalRatio
instance ToSchema Weights
instance ToSchema ReachWeight
instance ToSchema EffortWeight
instance ToSchema DistanceWeight
instance ToSchema LeadingWeight
instance ToSchema ArrivalWeight
instance ToSchema TimeWeight
instance ToSchema EssNotGoal
instance ToSchema Breakdown
instance ToSchema q => ToSchema (JumpedTheGun q)
instance ToSchema (PilotDistance q) => ToSchema (ReachToggle (PilotDistance q))
instance ToSchema Velocity
instance ToSchema q => ToSchema (PilotVelocity q)
instance ToSchema DfNoTrackPilot
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
instance ToSchema ZoneTag
instance ToSchema InterpolatedFix
instance ToSchema RawAlt
instance ToSchema ZoneCross
instance ToSchema Fix
instance ToSchema TrackCross
instance ToSchema TrackReach
instance ToSchema TrackArrival
instance ToSchema q => ToSchema (ArrivalLag q)
instance ToSchema ArrivalPlacing
instance (ToSchema a, ToSchema b) => ToSchema (LeadingAreas a b)
instance (u ~ LeadingArea2Units) => ToSchema (TrackLead u)
instance ToSchema TrackSpeed
instance ToSchema TrackEffort
instance ToSchema TickRow
instance ToSchema FixIdx
instance ToSchema ZoneIdx
instance ToSchema LegIdx
instance ToSchema LeadTick
instance ToSchema RaceTick
instance ToSchema EssTime
instance ToSchema RaceTime
instance ToSchema FirstLead
instance ToSchema FirstStart
instance ToSchema LastArrival

instance ToSchema RawLatLngTrack
instance ToSchema RawLeadingArea
instance ToSchema BolsterStats

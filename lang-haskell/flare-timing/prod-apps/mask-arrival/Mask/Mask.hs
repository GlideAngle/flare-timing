{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask.Mask (writeMask, check) where

import Control.Lens ((^?), element)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Monad.Except (MonadIO)
import Data.UnitsOfMeasure (u)

import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Zone.Raw (Give)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Clip (FlyCut(..), FlyClipping(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Comp(..)
    , Pilot(..)
    , Task(..)
    , Tweak(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , compToMaskArrival
    )
import Flight.Distance (QTaskDistance)
import Flight.Mask (GeoDash(..), FnIxTask, checkTracks)
import Flight.Track.Tag (Tagging)
import Flight.Track.Lead (LeadingAreaSum, MkLeadingCoef, MkAreaToCoef)
import Flight.Track.Arrival (TrackArrival(..), arrivalsByTime, arrivalsByRank)
import qualified Flight.Track.Arrival as Arrival (TrackArrival(..))
import Flight.Track.Distance (TrackDistance(..))
import Flight.Kml (LatLngAlt(..), MarkedFixes(..))
import Flight.Lookup.Stop (ScoredLookup(..))
import qualified Flight.Lookup as Lookup
    ( scoredTimeRange, arrivalRank, ticked, compRoutes
    , pilotTime, pilotEssTime
    )
import Flight.Lookup.Tag
    ( TaskLeadingLookup(..)
    , tagArrivalRank
    , tagPilotTime
    , tagTicked
    )
import Flight.Scribe (writeMaskingArrival)
import "flight-gap-allot" Flight.Score (ArrivalFraction(..))
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), DashPathInputs(..), nullStats, altToAlt)
import MaskArrival (maskArrival, arrivalInputs)
import MaskPilots (maskPilots)

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

writeMask
    :: LeadingAreaSum u
    -> MkLeadingCoef u
    -> MkAreaToCoef v
    -> Math
    -> CompSettings k
    -> RoutesLookupTaskDistance
    -> TaskLeadingLookup
    -> [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> IO
            [
                [Either
                    (Pilot, TrackFileFail)
                    (Pilot, Pilot -> FlightStats k)
                ]
            ])
    -> IO ()
writeMask
    _sumAreas
    _invert
    _areaToCoef
    _math
    CompSettings
        { nominal = Cmp.Nominal{free}
        , tasks
        , pilotGroups
        }
    routes
    _lookupTaskLeading
    selectTasks selectPilots compFile f = do

    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."

        Just fss -> do

            let iTasks = IxTask <$> [1 .. length fss]

            -- Task lengths (ls).
            let lsTask' = Lookup.compRoutes routes iTasks

            let yss = maskPilots free tasks lsTask' pilotGroups fss

            -- Arrivals (as).
            let as :: [[(Pilot, TrackArrival)]] =
                    [
                        let aRank = maybe True arrivalRank tweak
                            aTime = maybe False arrivalTime tweak
                            ys' = arrivalInputs ys
                        in
                            case (aRank, aTime) of
                                (True, _) -> arrivalsByRank ys'
                                (False, True) -> arrivalsByTime ys'
                                -- NOTE: We're not using either kind of arrival
                                -- for points so zero the fraction.
                                (False, False) ->
                                    [ (p, ta{Arrival.frac = ArrivalFraction 0})
                                    | (p, ta) <- arrivalsByRank ys'
                                    ]

                    | Task{taskTweak = tweak} <- tasks
                    | ys <- yss
                    ]

            -- REVIEW: Waiting on feedback on GAP rule question about altitude
            -- bonus distance pushing a flight to goal so that it arrives.
            writeMaskingArrival (compToMaskArrival compFile) (maskArrival as)

check
    :: (MonadThrow m, MonadIO m)
    => Math
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> m [[Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)]]
check math lengths flying tags =
    checkTracks $ \CompSettings{tasks, comp = Comp{earthMath, give}} ->
        flown math earthMath give lengths flying tags tasks

flown
    :: Math
    -> EarthMath
    -> Maybe Give
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe Tagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown math earthMath give (RoutesLookupTaskDistance lookupTaskLength) flying tags tasks iTask fixes =
    maybe
        (const nullStats)
        (\d -> flown' d flying math earthMath give tags tasks iTask fixes)
        taskLength
    where
        taskLength = (fmap wholeTaskDistance . ($ iTask)) =<< lookupTaskLength

flown'
    :: QTaskDistance Double [u| m |]
    -> ScoredLookup
    -> Math
    -> EarthMath
    -> Maybe Give
    -> Maybe Tagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown' _ _ Rational _ _ _ _ _ _ _ = error "Nigh for rationals not yet implemented."
flown' dTaskF flying Floating earthMath give tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
    case maybeTask of
        Nothing -> nullStats

        Just task' ->
            case (ssTime, gsTime, esTime, arrivalRank) of
                (Just a, Just b, Just e, c@(Just _)) ->
                    tickedStats {statTimeRank = Just $ TimeStats a b e c}

                _ ->
                    tickedStats
                        { statLand = Just $ landDistance task'
                        , statAlt =
                            case reverse ys of
                                [] -> Nothing
                                y : _ -> Just . altToAlt $ altGps y
                        }

    where
        maybeTask = tasks ^? element (i - 1)

        ticked = Lookup.ticked (tagTicked tags) mf iTask speedSection' p
        (_, esTime) = Lookup.pilotEssTime (tagPilotTime tags) mf iTask [] speedSection' p
        ssTime = Lookup.pilotTime (tagPilotTime tags) mf iTask [] speedSection' p
        gsTime = Lookup.pilotTime (tagPilotTime tags) mf iTask startGates' speedSection' p
        arrivalRank = Lookup.arrivalRank (tagArrivalRank tags) mf iTask speedSection' p
        FlyCut{uncut = MarkedFixes{fixes = ys}} = clipToCut xs

        xs =
            FlyCut
                { cut = Lookup.scoredTimeRange flying mark0 iTask p
                , uncut = mf
                }

        tickedStats =
            nullStats
                { statDash =
                    DashPathInputs
                        { dashTask = maybeTask
                        , dashTicked = ticked
                        , dashFlyCut = Just xs
                        }
                }

        landDistance task =
            let earth =
                    ( earthMath
                    , let e = EarthAsEllipsoid wgs84 in case earthMath of
                          Pythagorus -> EarthAsFlat UTM
                          Haversines -> EarthAsSphere earthRadius
                          Vincenty -> e
                          AndoyerLambert -> e
                          ForsytheAndoyerLambert -> e
                          FsAndoyer -> e
                    )
            in
                TrackDistance
                    { togo =
                        togoAtLanding @Double @Double
                            earth
                            give
                            sp
                            ticked
                            task
                            xs

                    , made =
                        madeAtLanding @Double @Double
                            earth
                            give
                            sp
                            dTaskF
                            ticked
                            task
                            xs
                    }

        startGates' =
            case tasks ^? element (i - 1) of
                Nothing -> []
                Just Task{..} -> startGates

        speedSection' =
            case tasks ^? element (i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

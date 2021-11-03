{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Time.Align
    ( checkAll
    , writeTime
    , group
    ) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Lens ((^?), element)
import Data.These
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Control.Monad (mapM_, when, zipWithM_)
import Control.Exception.Safe (catchIO)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Flight.Clip (FlyingSection, FlyCut(..))
#if __EARTH_RAT__
import Flight.LatLng.Rational (defEps)
import qualified Flight.Span.Rational as Rat (fromR)
#endif
import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Zone.Raw (Give)
import Flight.Track.Time
    ( FixIdx(..), ZoneIdx(..), LegIdx(..), LeadTick(..), RaceTick(..), TimeRow(..)
    , commentOnFixRange
    )
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(UTM))
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Comp
    ( AlignTimeDir(..)
    , ScoringInputFiles
    , CompInputFile(..)
    , AlignTimeFile(..)
    , CompTaskSettings(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail
    , FirstLead(..)
    , FirstStart(..)
    , OpenClose(..)
    , compFileToCompDir
    , alignTimePath
    , openClose
    , timeCheck
    )
import Flight.Mask
    ( GeoDash(..), FnIxTask, RaceSections(..), GroupLeg(..), Ticked
    , checkTracks, groupByLeg, dashDistancesToGoal
    )
import Flight.Track.Cross (Fix(..), ZoneTag(..), asIfFix)
import Flight.Track.Tag (CompTagging(..), TrackTime(..), firstLead, firstStart)
import Flight.Track.Stop (TrackScoredSection(..))
import Flight.Kml (MarkedFixes(..), timeToFixIdx)
import Data.Ratio.Rounding (dpRound)
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Scribe (writeAlignTime)
import Flight.Lookup.Stop(ScoredLookup(..))
import Flight.Lookup.Tag
    (TickLookup(..), TagLookup(..), tagTicked, tagPilotTag)
import Flight.Span.Math (Math(..))

unTaskDistance :: QTaskDistance Double [u| m |] -> Double
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

writeTime
    :: [IxTask]
    -> [Pilot]
    -> ScoringInputFiles
    -> (ScoringInputFiles
      -> [IxTask]
      -> [Pilot]
      -> IO [[Either (Pilot, t) (Pilot, Pilot -> [TimeRow])]])
    -> IO ()
writeTime selectTasks selectPilots inFiles@(compFile, _) f = do
    checks <-
        catchIO
            (Just <$> f inFiles selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just xs -> do
            let ys :: [[(Pilot, [TimeRow])]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, [])
                            Right (p, g) -> (p, g p))
                        xs

            zipWithM_
                (\ixTask zs ->
                    when (includeTask selectTasks ixTask) $
                        mapM_ (writePilotTimes compFile ixTask) zs)
                (IxTask <$> [1 .. ])
                ys

checkAll
    :: Math
    -> EarthMath
    -> Maybe Give
    -> SampleParams Double
    -> Bool -- ^ Exclude zones outside speed section
    -> ScoredLookup
    -> CompTagging
    -> ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> IO
         [
             [Either
                 (Pilot, TrackFileFail)
                 (Pilot, Pilot -> [TimeRow])
             ]
         ]
checkAll math earthMath give sp ssOnly scoredLookup tagging =
    checkTracks
        (\CompTaskSettings{tasks} -> group math earthMath give sp ssOnly scoredLookup tagging tasks)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

writePilotTimes :: CompInputFile -> IxTask -> (Pilot, [TimeRow]) -> IO ()
writePilotTimes compFile ixTask (pilot, rows) = do
    putStrLn . commentOnFixRange pilot $ fixIdx <$> rows
    _ <- createDirectoryIfMissing True dOut
    _ <- writeAlignTime (AlignTimeFile $ dOut </> f) rows
    return ()
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dOut, AlignTimeFile f) = alignTimePath dir ixTask pilot

mkTimeRows
    :: Maybe FirstLead
    -> Maybe FirstStart
    -> LegIdx
    -> Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
    -> [TimeRow]
mkTimeRows _ _ _ Nothing = []
mkTimeRows lead start leg (Just xs) =
    catMaybes
    [ mkTimeRow lead start leg fixIdx fix d
    | fixIdx <- FixIdx <$> [1..]
    | fix <- fst <$> xs
    | d <- snd <$> xs
    ]

mkTimeRow
    :: Maybe FirstLead
    -> Maybe FirstStart
    -> LegIdx
    -> FixIdx
    -> Maybe Fix
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe TimeRow
mkTimeRow Nothing _ _ _ _ _ = Nothing
mkTimeRow _ _ _ _ Nothing _ = Nothing
mkTimeRow _ _ _ _ _ Nothing = Nothing
mkTimeRow lead start legIdx fixIdx (Just Fix{..}) (Just d) =
    Just
        TimeRow
            { fixIdx = fixIdx
            , zoneIdx = ZoneIdx fix
            , legIdx = legIdx

            , tickLead =
                LeadTick
                . realToFrac
                . (\(FirstLead l) -> time `diffUTCTime` l)
                <$> lead

            , tickRace =
                RaceTick
                . realToFrac
                . (\(FirstStart s) -> time `diffUTCTime` s)
                <$> start

            , time = time
            , lat = lat
            , lng = lng
            , alt = alt
            , togo = unTaskDistance d
            }

group
    :: Math
    -> EarthMath
    -> Maybe Give
    -> SampleParams Double
    -> Bool -- ^ Exclude zones outside speed section
    -> ScoredLookup
    -> CompTagging
    -> FnIxTask k (Pilot -> [TimeRow])
group
    math
    earthMath
    give
    sp
    ssOnly
    (ScoredLookup lookupScored)
    tags@CompTagging{timing}
    tasks iTask@(IxTask i)
    mf@MarkedFixes{mark0} p =
    case (tasks ^? element (i - 1), timing ^? element (i - 1)) of
        (_, Nothing) -> []
        (Nothing, _) -> []
        (Just Task{speedSection = Nothing}, _) -> []
        (Just
            task@Task
                { speedSection = ss@(Just (start, end))
                , zoneTimes
                , startGates
                , earlyStart
                }, Just times) ->
            maybe
                zs
                ( maybe zs (\z -> zs ++ [z])
                . (\f ->
                    mkTimeRow
                        firstLead'
                        firstStart'
                        (LegIdx end)
                        (FixIdx 0)
                        (Just f)
                        (Just $ TaskDistance [u| 0m |]))
                )
                (asIfFix <$> endZoneTag)
            where
                scoredTimeRange :: FlyingSection UTCTime =
                    fromMaybe
                        (Just (mark0, mark0))
                        (fmap scoredTimes . (\f -> f iTask p) =<< lookupScored)

                -- NOTE: Ensure we're only considering scored subset of flying time.
                scoredMarkedFixes =
                    FlyCut
                        { cut = scoredTimeRange
                        , uncut = mf
                        }

                firstTimes = zonesFirst times

                firstLead' = firstLead ss firstTimes

                firstStart' =
                    (\OpenClose{open} -> firstStart ss open firstTimes)
                    =<< openClose ss zoneTimes

                xs :: [(Maybe GroupLeg, MarkedFixes)]
                xs =
                    case math of
                        Floating ->
                            groupByLeg @Double @Double
                                ( earthMath
                                , let e = EarthAsEllipsoid wgs84 in case earthMath of
                                      Pythagorus -> EarthAsFlat UTM
                                      Haversines -> EarthAsSphere earthRadius
                                      Vincenty -> e
                                      AndoyerLambert -> e
                                      ForsytheAndoyerLambert -> e
                                      FsAndoyer -> e
                                )
                                give
                                sp
                                (timeCheck earlyStart startGates zoneTimes)
                                task
                                scoredMarkedFixes
                        Rational -> error "Grouping for rational math is not yet implemented."

                yss = fmap (FlyCut scoredTimeRange) <$> xs

                ticked =
                    fromMaybe (RaceSections [] [] [])
                    $ (\f -> f iTask ss p mf) =<< lookupTicked

                endZoneTag :: Maybe ZoneTag
                endZoneTag = do
                    ts :: [Maybe ZoneTag]
                        <- (\f -> f iTask ss p mf) =<< lookupZoneTags

                    us :: [ZoneTag]
                        <- sequence ts

                    listToMaybe . take 1 . drop (end - start) $ us

                zs' :: [TimeRow]
                zs' =
                    let f = legDistances math earthMath give sp ssOnly in
                    concat
                    [
                        case leg of
                            Nothing ->
                                -- NOTE: Pilots not tagging any zones. This can
                                -- happen when they launch outside the launch
                                -- zone and fail to tag the start zone.
                                let legL = LegIdx 0
                                    (_, reticked) = retick ticked (LegIdx start) legL
                                in f reticked times task legL ys
                            Just GroupLeg{groupLeg = That (LegIdx legR)} ->
                                let legL = LegIdx $ legR - 1
                                    (_, reticked) = retick ticked (LegIdx start) legL
                                in f reticked times task legL ys
                            Just GroupLeg{groupLeg = These legL _} ->
                                let (_, reticked) = retick ticked (LegIdx start) legL
                                in f reticked times task legL ys
                            Just GroupLeg{groupLeg = This legL} ->
                                let (_, reticked) = retick ticked (LegIdx start) legL in
                                f reticked times task legL ys

                    | (leg, ys) <- yss
                    ]

                -- REVIEW: This lookup from time to fix is slow but correct.
                zs :: [TimeRow] =
                    [ z{fixIdx = FixIdx . fromMaybe 0 $ (+ 1) <$> timeToFixIdx t mf}
                    | z@TimeRow{time = t} <- zs'
                    ]
    where
        (TickLookup lookupTicked) = tagTicked (Just tags)
        (TagLookup lookupZoneTags) = tagPilotTag (Just tags)

-- | For a given leg, only so many race zones can be ticked.
retick :: Ticked -> LegIdx -> LegIdx -> (LegIdx, Ticked)
retick rs@RaceSections{prolog, race} (LegIdx start) (LegIdx leg) =
    (LegIdx $ leg + delta, rs')
    where
        -- NOTE: Some pilots get towed up outside the start circle. Make an
        -- adjustment between the start and the zones in the prolog ticked.
        delta = (start - 1) - length prolog
        rs' = rs { race = take (leg - start + 1) race }

allLegDistances
    :: Math
    -> EarthMath
    -> Maybe Give
    -> SampleParams Double
    -> Ticked
    -> TrackTime
    -> Task k
    -> LegIdx
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]

allLegDistances Floating earthMath give sp ticked times task@Task{speedSection, zoneTimes} leg xs =
    mkTimeRows lead start leg xs'
    where
        xs' :: Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
        xs' =
            dashDistancesToGoal @Double @Double
                ( earthMath
                , let e = EarthAsEllipsoid wgs84 in case earthMath of
                      Pythagorus -> EarthAsFlat UTM
                      Haversines -> EarthAsSphere earthRadius
                      Vincenty -> e
                      AndoyerLambert -> e
                      ForsytheAndoyerLambert -> e
                      FsAndoyer -> e
                )
                give
                sp
                ticked
                task
                xs

        ts = zonesFirst times
        lead = firstLead speedSection ts
        start =
            (\OpenClose{open} -> firstStart speedSection open ts)
            =<< openClose speedSection zoneTimes

#if __EARTH_RAT__
allLegDistances Rational earthMath give sp ticked times task@Task{speedSection, zoneTimes} leg xs =
    mkTimeRows lead start leg xs'
    where
        xs' :: Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
        xs' =
            (fmap . fmap . fmap . fmap) Rat.fromR $
            dashDistancesToGoal
                ( earthMath
                , let e = EarthAsEllipsoid wgs84 in case earthMath of
                      Pythagorus -> EarthAsFlat UTM
                      Haversines -> EarthAsSphere earthRadius
                      Vincenty -> e
                      AndoyerLambert -> e
                      ForsytheAndoyerLambert -> e
                      FsAndoyer -> e
                , defEps
                )
                give
                sp
                ticked
                task
                xs

        ts = zonesFirst times
        lead = firstLead speedSection ts
        start =
            (\OpenClose{open} -> firstStart speedSection open ts)
            =<< openClose speedSection zoneTimes
#else
allLegDistances Rational _ _ _ _ _ _ _ _ = error "Leg distances for rational math is not yet implemented."
#endif

legDistances
    :: Math
    -> EarthMath
    -> Maybe Give
    -> SampleParams Double
    -> Bool -- ^ Exclude zones outside speed section
    -> Ticked
    -> TrackTime
    -> Task k
    -> LegIdx
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
legDistances math earthMath give sp False ticked times task leg xs=
    allLegDistances math earthMath give sp ticked times task leg xs

legDistances math earthMath give sp True ticked times task@Task{speedSection} leg xs =
    if excludeLeg
       then []
       else allLegDistances math earthMath give sp ticked times task leg xs
    where
        LegIdx leg' = leg

        excludeLeg =
            maybe
                False
                (\(start, end) -> leg' < start || leg' > end)
                speedSection

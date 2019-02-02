{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Time.Align
    ( checkAll
    , writeTime
    , group
    ) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Lens ((^?), element)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Control.Monad (mapM_, when, zipWithM_)
import Control.Exception.Safe (catchIO)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Flight.Clip (FlyingSection, FlyCut(..))
import Flight.Track.Time
    ( FixIdx(..), ZoneIdx(..), LegIdx(..), LeadTick(..), RaceTick(..), TimeRow(..)
    , allHeaders, commentOnFixRange
    )
import Flight.Comp
    ( AlignTimeDir(..)
    , CompInputFile(..)
    , AlignTimeFile(..)
    , CompSettings(..)
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
    )
import qualified Flight.Mask as Mask (Sliver(..))
import Flight.Mask
    ( FnIxTask, RaceSections(..), Ticked
    , checkTracks, groupByLeg, dashDistancesToGoal
    )
import Flight.Track.Cross (Fix(..), TrackFlyingSection(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), firstLead, firstStart)
import Flight.Kml (MarkedFixes(..), timeToFixIdx)
import Data.Ratio.Rounding (dpRound)
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Scribe (writeAlignTime)
import Flight.Lookup.Cross (FlyingLookup(..))
import Flight.Lookup.Tag
    (TickLookup(..), TagLookup(..), tagTicked, tagPilotTag)
import Flight.Span.Double (zoneToCylF, spanF, csF, cutF, dppF, csegF)

unTaskDistance :: QTaskDistance Double [u| m |] -> Double
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

writeTime
    :: [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> (CompInputFile
      -> [IxTask]
      -> [Pilot]
      -> IO [[Either (Pilot, t) (Pilot, Pilot -> [TimeRow])]])
    -> IO ()
writeTime selectTasks selectPilots compFile f = do
    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
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
                (\ n zs ->
                    when (includeTask selectTasks $ IxTask n) $
                        mapM_ (writePilotTimes compFile n) zs)
                [1 .. ]
                ys

checkAll
    :: Bool -- ^ Exclude zones outside speed section
    -> FlyingLookup
    -> Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO
         [
             [Either
                 (Pilot, TrackFileFail)
                 (Pilot, Pilot -> [TimeRow])
             ]
         ]
checkAll ssOnly flyingLookup tagging =
    checkTracks (\CompSettings{tasks} -> group ssOnly flyingLookup tagging tasks)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

writePilotTimes :: CompInputFile -> Int -> (Pilot, [TimeRow]) -> IO ()
writePilotTimes compFile iTask (pilot, rows) = do
    putStrLn . commentOnFixRange pilot $ fixIdx <$> rows
    _ <- createDirectoryIfMissing True dOut
    _ <- writeAlignTime (AlignTimeFile $ dOut </> f) allHeaders rows
    return ()
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dOut, AlignTimeFile f) = alignTimePath dir iTask pilot

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
mkTimeRow lead start legIdx fixIdx (Just Fix{fix, time, lat, lng}) (Just d) =
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
            , distance = unTaskDistance d
            }

group
    :: Bool -- ^ Exclude zones outside speed section
    -> FlyingLookup
    -> Tagging
    -> FnIxTask k (Pilot -> [TimeRow])
group
    ssOnly
    (FlyingLookup lookupFlying)
    tags@Tagging{timing}
    tasks iTask@(IxTask i)
    mf@MarkedFixes{mark0} p =
    case (tasks ^? element (i - 1), timing ^? element (i - 1)) of
        (_, Nothing) -> []
        (Nothing, _) -> []
        (Just Task{speedSection = Nothing}, _) -> []
        (Just task@Task{speedSection = ss@(Just (start, end))}, Just times) ->
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
                endZoneTag
            where
                scoredTimeRange :: FlyingSection UTCTime =
                    fromMaybe
                        (Just (mark0, mark0))
                        (fmap scoredTimes . (\f -> f iTask p) =<< lookupFlying)

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
                    =<< openClose ss (zoneTimes task)

                xs :: [(LegIdx, MarkedFixes)]
                xs = groupByLeg spanF zoneToCylF task scoredMarkedFixes

                yss = (fmap $ FlyCut scoredTimeRange) <$> xs

                ticked =
                    fromMaybe (RaceSections [] [] [])
                    $ (\f -> f iTask ss p mf) =<< lookupTicked

                endZoneTag :: Maybe Fix
                endZoneTag = do
                    ts :: [Maybe Fix]
                        <- (\f -> f iTask ss p mf) =<< lookupZoneTags

                    us :: [Fix]
                        <- sequence ts

                    listToMaybe . take 1 . drop (end - start) $ us

                zs' :: [TimeRow]
                zs' =
                    concat
                    [
                        let (_, reticked) = retick ticked (LegIdx start) leg in
                        legDistances ssOnly reticked times task leg ys
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
    :: Ticked
    -> TrackTime
    -> Task k
    -> LegIdx
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
allLegDistances ticked times task@Task{speedSection, zoneTimes} leg xs =
    mkTimeRows lead start leg xs'
    where
        xs' :: Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
        xs' = dashDistancesToGoal ticked sliver zoneToCylF task xs

        sliver = Mask.Sliver spanF dppF csegF csF cutF
        ts = zonesFirst times

        lead = firstLead speedSection ts

        start =
            (\OpenClose{open} -> firstStart speedSection open ts)
            =<< openClose speedSection zoneTimes

legDistances
    :: Bool -- ^ Exclude zones outside speed section
    -> Ticked
    -> TrackTime
    -> Task k
    -> LegIdx
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
legDistances False ticked times task leg xs=
    allLegDistances ticked times task leg xs

legDistances True ticked times task@Task{speedSection} leg xs =
    if excludeLeg then [] else allLegDistances ticked times task leg xs
    where
        LegIdx leg' = leg

        excludeLeg =
            maybe
                False
                (\(start, end) -> leg' < start || leg' > end)
                speedSection
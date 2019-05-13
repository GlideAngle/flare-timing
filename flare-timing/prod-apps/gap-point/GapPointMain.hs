{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.Ratio ((%))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import Data.Time.Clock (UTCTime)
import System.Clock (getTime, Clock(Monotonic))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.List (sortBy, partition)
import Control.Applicative (liftA2)
import qualified Control.Applicative as A ((<$>))
import Control.Monad (mapM_, join)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import Data.UnitsOfMeasure ((/:), u, convert, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Distance (QTaskDistance, TaskDistance(..), unTaskDistanceAsKm)
import Flight.Route (OptimalRoute(..))
import qualified Flight.Comp as Cmp (DfNoTrackPilot(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , Tweak(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , Pilot
    , PilotGroup(dnf, didFlyNoTracklog)
    , StartGate(..)
    , StartEnd(..)
    , Task(..)
    , TaskStop(..)
    , DfNoTrack(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , IxTask(..)
    , compToTaskLength
    , compToCross
    , crossToTag
    , compToMask
    , compToLand
    , compToPoint
    , findCompInput
    , ensureExt
    )
import Flight.Track.Cross
    (InterpolatedFix(..), Crossing(..), ZoneTag(..), TrackFlyingSection(..))
import Flight.Track.Tag (Tagging(..), PilotTrackTag(..), TrackTag(..))
import Flight.Track.Distance
    ( TrackDistance(..), AwardedDistance(..), Clamp(..), Nigh, Land
    , awardByFrac
    )
import Flight.Track.Time (AwardedVelocity(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Speed (pilotTime, startGateTaken)
import qualified Flight.Track.Speed as Speed (TrackSpeed(..))
import Flight.Track.Mask (Masking(..))
import Flight.Track.Land (Landing(..))
import Flight.Track.Place (rankByTotal)
import Flight.Track.Point
    (Velocity(..), Breakdown(..), Pointing(..), Allocation(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    ( readComp, readRoute
    , readCrossing, readTagging
    , readMasking, readLanding
    , writePointing
    )
import Flight.Mask (RaceSections(..), section)
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Zone.MkZones (Discipline(..))
import Flight.Lookup.Route (routeLength)
import qualified Flight.Lookup as Lookup (compRoutes)
import Flight.Score
    ( MinimumDistance(..), MaximumDistance(..), LaunchToEss(..)
    , BestDistance(..), SumOfDistance(..), PilotDistance(..)
    , PilotsAtEss(..), PilotsPresent(..), PilotsFlying(..), PilotsLanded(..)
    , GoalRatio(..), Lw(..), Aw(..), Rw(..), Ew(..)
    , NominalTime(..), BestTime(..)
    , Validity(..), ValidityWorking(..), StopValidity(..)
    , DifficultyFraction(..), LeadingFraction(..)
    , ArrivalFraction(..), SpeedFraction(..)
    , DistancePoints(..), LinearPoints(..), DifficultyPoints(..)
    , LeadingPoints(..), ArrivalPoints(..), TimePoints(..)
    , PointPenalty
    , TaskPlacing(..), TaskPoints(..), PilotVelocity(..), PilotTime(..)
    , IxChunk(..), ChunkDifficulty(..)
    , FlownMean(..), FlownStdDev(..)
    , distanceRatio, distanceWeight, reachWeight, effortWeight
    , leadingWeight, arrivalWeight, timeWeight
    , taskValidity, launchValidity, distanceValidity, timeValidity, stopValidity
    , availablePoints, applyPointPenalties
    , toIxChunk
    )
import qualified Flight.Score as Gap (Validity(..), Points(..), Weights(..))
import GapPointOptions (description)

type StartEndTags = StartEnd (Maybe ZoneTag) ZoneTag

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    Fmt.fprint ("Tallying points completed in " Fmt.% timeSpecs Fmt.% "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile@(LandOutFile landPath) = compToLand compFile
    let pointFile = compToPoint compFile
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading pilots ABS & DNF from task from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading zone crossings from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading start and end zone tagging from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading masked tracks from '" ++ takeFileName maskPath ++ "'"
    putStrLn $ "Reading distance difficulty from '" ++ takeFileName landPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    cgs <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    tgs <-
        catchIO
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readMasking maskFile)
            (const $ return Nothing)

    landing <-
        catchIO
            (Just <$> readLanding landFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    let lookupTaskLength =
            routeLength
                taskRoute
                taskRouteSpeedSubset
                stopRoute
                routes

    case (compSettings, cgs, tgs, masking, landing, routes) of
        (Nothing, _, _, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _, _, _) -> putStrLn "Couldn't read the crossings."
        (_, _, Nothing, _, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, _, Nothing, _, _) -> putStrLn "Couldn't read the maskings."
        (_, _, _, _, Nothing, _) -> putStrLn "Couldn't read the land outs."
        (_, _, _, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just cs, Just cg, Just tg, Just mk, Just lg, Just _) ->
            writePointing pointFile $ points' cs lookupTaskLength cg tg mk lg

points'
    :: CompSettings k
    -> RoutesLookupTaskDistance
    -> Crossing
    -> Tagging
    -> Masking
    -> Cmp.Landing
    -> Pointing
points'
    CompSettings
        { comp =
            Comp{discipline}
        , nominal =
            Nominal
                { launch = lNom
                , goal = gNom
                , distance = dNom
                , time = tNom
                , free
                }
        , tasks
        , pilots
        , pilotGroups
        }
    routes
    Crossing{flying}
    Tagging{tagging}
    Masking
        { pilotsAtEss
        , taskSpeedDistance
        , bestDistance
        , sumDistance
        , ssBestTime
        , gsBestTime
        , leadRank
        , arrivalRank
        , flownMean
        , flownStdDev
        , ssSpeed
        , gsSpeed
        , nigh
        , land
        , altStopped
        }
    Landing
        { difficulty = landoutDifficulty
        } =
    Pointing
        { validityWorking = workings
        , validity = validities
        , allocation = allocs
        , score = score
        , scoreDf = scoreDf
        , scoreDfNoTrack = scoreDfNoTrack
        }
    where
        -- NOTE: p = pilot, t = track, nt = no track, dnf = did not fly, df = did fly
        -- s suffix is a list, ss suffix is a list of lists.
        pss = toInteger . length <$> pilots
        ntss = toInteger . length . unDfNoTrack . didFlyNoTracklog <$> pilotGroups
        dnfss = toInteger . length . dnf <$> pilotGroups

        tss =
            [ ps - (dnfs + nts)
            | ps <- pss
            | dnfs <- dnfss
            | nts <- ntss
            ]

        dfss =
            [ ts + nts
            | ts <- tss
            | nts <- ntss
            ]

        dfNtss = didFlyNoTracklog <$> pilotGroups

        -- Task lengths (ls).
        iTasks = IxTask <$> [1 .. length tss]
        lsTask' = Lookup.compRoutes routes iTasks

        lsWholeTask :: [Maybe (QTaskDistance Double [u| m |])] =
            (fmap . fmap) wholeTaskDistance lsTask'

        lsSpeedTask :: [Maybe (QTaskDistance Double [u| m |])] =
            (fmap . fmap) speedSubsetDistance lsTask'

        lsLaunchToEssTask :: [Maybe (QTaskDistance Double [u| m |])] =
            (fmap . fmap) launchToEssDistance lsTask'

        -- NOTE: If there is no best distance, then either the task wasn't run
        -- or it has not been scored yet.
        maybeTasks :: [a -> Maybe a]
        maybeTasks =
            [ if null ds then const Nothing else Just | ds <- bestDistance ]

        lvs =
            [ launchValidity
                lNom
                (PilotsPresent . fromInteger $ dfs + dnfs)
                (PilotsFlying . fromInteger $ dfs)
            | dfs <- dfss
            | dnfs <- dnfss
            ]

        dBests :: [MaximumDistance (Quantity Double [u| km |])] =
            [ MaximumDistance . MkQuantity $ fromMaybe 0 b
            | b <- (fmap . fmap) unTaskDistanceAsKm bestDistance
            ]

        dSums :: [SumOfDistance (Quantity Double [u| km |])] =
            [ SumOfDistance . MkQuantity $ fromMaybe 0 s
            | s <- (fmap . fmap) unTaskDistanceAsKm sumDistance
            ]

        dvs =
            [ distanceValidity
                gNom
                dNom
                pf
                free
                b
                s
            | pf <- PilotsFlying <$> dfss
            | b <- dBests
            | s <- dSums
            ]

        tvs =
            let f =
                    (fmap . fmap)
                        (\(BestTime x) -> BestTime (convert x :: Quantity _ [u| s |]))
            in
                [ timeValidity
                    ((\(NominalTime x) ->
                        NominalTime (convert x :: Quantity _ [u| s |])) tNom)
                    ssT
                    gsT
                    dNom
                    d

                | ssT <- f ssBestTime
                | gsT <- f gsBestTime
                | d <- (\(MaximumDistance x) -> BestDistance x) <$> dBests
                ]

        workings :: [Maybe ValidityWorking] =
            [ do
                lv' <- lv
                dv' <- dv
                (flip (ValidityWorking lv' dv') Nothing) <$> tv
            | lv <- snd <$> lvs
            | dv <- snd <$> dvs
            | tv <- snd <$> tvs
            ]

        grs =
            [ GoalRatio $ n % dfs
            | n <- (\(PilotsAtEss x) -> x) <$> pilotsAtEss
            | dfs <- dfss
            ]

        dws = distanceWeight <$> grs

        rws =
            let rw = if discipline == HangGliding then RwHg else RwPg
            in reachWeight . rw <$> dws

        ews =
            if discipline == HangGliding
               then effortWeight . EwHg <$> dws
               else const (effortWeight EwPg) <$> dws

        lws =
            [
                leadingWeight $
                maybe
                    (if | discipline == HangGliding -> LwHg dw
                        | gr == GoalRatio 0 -> LwPgZ $ distanceRatio bd td
                        | otherwise -> LwPg dw)
                    (\k ->
                        if | discipline == HangGliding -> LwScaled k dw
                           | gr == GoalRatio 0 -> LwPgZ $ distanceRatio bd td
                           | otherwise -> LwScaled k dw)
                    (join $ leadingWeightScaling <$> tw)

            | gr <- grs
            | dw <- dws
            | tw <- taskTweak <$> tasks
            | bd <- maybe [u| 0.0 km |] (MkQuantity . unTaskDistanceAsKm) <$> bestDistance
            | td <- maybe [u| 0.0 km |] (MkQuantity . unTaskDistanceAsKm) <$> lsWholeTask
            ]

        aws =
            [
                maybe
                    (if discipline == HangGliding
                        then arrivalWeight (AwHg dw)
                        else arrivalWeight AwPg)
                    (\k -> arrivalWeight (AwScaled k dw))
                    (join $ arrivalWeightScaling <$> tw)

            | dw <- dws
            | tw <- taskTweak <$> tasks
            ]

        ws =
            [ Gap.Weights dw rw ew lw aw (timeWeight dw lw aw)
            | dw <- dws
            | rw <- rws
            | ew <- ews
            | lw <- lws
            | aw <- aws
            ]

        pfss :: [[(Pilot, Maybe UTCTime)]] =
            [
                [ (p, join $ (fmap snd . flyingTimes) <$> tfs)
                | (p, tfs) <- fts
                ]
            | fts <- flying
            ]

        pls :: [([(Pilot, Maybe UTCTime)], [(Pilot, Maybe UTCTime)])] =
            [
                case stopped of
                    Nothing -> ([], [])
                    Just TaskStop{retroactive = t} ->
                        partition
                            ((maybe True (< t)) . snd)
                            pfs

            | pfs <- pfss
            | Task{stopped} <-tasks
            ]

        svs :: [Maybe StopValidity] =
            [
                do
                    _ <- sp
                    ed' <- ed
                    let pl = PilotsLanded . fromIntegral $ length landedByStop
                    return $ stopValidity pf pe pl fm fsd bd ed'

            | sp <- stopped <$> tasks
            | pf <- PilotsFlying <$> dfss
            | pe <- pilotsAtEss
            | landedByStop <- (fmap snd . fst) <$> pls
            | fm <- (\(TaskDistance td) -> FlownMean $ convert td) <$> flownMean
            | fsd <- (\(TaskDistance td) -> FlownStdDev $ convert td) <$> flownStdDev
            | bd <- (\(MaximumDistance x) -> BestDistance x) <$> dBests
            | ed <- (fmap . fmap) (\(TaskDistance td) -> LaunchToEss $ convert td) lsLaunchToEssTask
            ]

        validities :: [Maybe Validity] =
            [ maybeTask $ Validity (taskValidity lv dv tv sv) lv dv tv sv
            | lv <- fst <$> lvs
            | dv <- fst <$> dvs
            | tv <- fst <$> tvs
            | sv <- svs
            | maybeTask <- maybeTasks
            ]

        allocs :: [Maybe Allocation]=
            [ do
                v' <- v
                let (pts, taskPoints) = availablePoints v' w
                return $ Allocation gr w pts taskPoints
            | gr <- grs
            | w <- ws
            | v <- (fmap . fmap) Gap.task validities
            ]

        -- NOTE: Pilots either get to goal or have a nigh distance.
        nighDistanceDf :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeNigh xs
                  ys' = (fmap . fmap) (const bd) ys
              in (xs' ++ ys')
            | bd <- (fmap . fmap) unTaskDistanceAsKm bestDistance
            | xs <- nigh
            | ys <- arrivalRank
            ]

        nighDistanceDfNoTrack :: [[(Pilot, Maybe Double)]] =
            [
                (\Cmp.DfNoTrackPilot{pilot = p, awardedReach = aw} ->
                    (p, madeAwarded free lWholeTask aw))
                <$> xs
            | DfNoTrack xs <- dfNtss
            | lWholeTask <- lsWholeTask
            ]

        -- NOTE: Pilots either get to the end of the speed section or
        -- they don't and will not get a speed over that section.
        speedDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) (const Nothing) xs
                  ys' = (fmap . fmap) (const sd) ys
              in (xs' ++ ys')
            | sd <- (fmap . fmap) unTaskDistanceAsKm taskSpeedDistance
            | xs <- nigh
            | ys <- arrivalRank
            ]

        -- NOTE: Pilots either get to goal or have a landing distance.
        landDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeLand xs
                  ys' = (fmap . fmap) (const bd) ys
              in (xs' ++ ys')
            | bd <- (fmap . fmap) unTaskDistanceAsKm bestDistance
            | xs <- land
            | ys <- arrivalRank
            ]

        stoppedAlts :: [[(Pilot, Maybe (QAlt Double [u| m |]))]] =
            [ let ys' = (fmap . fmap) (const Nothing) ys in (xs ++ ys')
            | xs <- (fmap . fmap . fmap) Just altStopped
            | ys <- arrivalRank
            ]

        difficultyDistancePointsDf :: [[(Pilot, DifficultyPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let ld' = mapOfDifficulty ld

                        (f, g) = discipline & \case
                               HangGliding ->
                                    (madeDifficultyDf free ld', const $ DifficultyFraction 0.5)
                               Paragliding ->
                                    (const $ DifficultyFraction 0.0, const $ DifficultyFraction 0.0)

                        xs' = (fmap . fmap) f xs
                        ys' = (fmap . fmap) g ys
                    in
                        (fmap . fmap)
                        (applyDifficulty ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- land
            | ys <- arrivalRank
            | ld <- landoutDifficulty
            ]

        difficultyDistancePointsDfNoTrack :: [[(Pilot, DifficultyPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let ld' = mapOfDifficulty ld

                        f = discipline & \case
                               HangGliding -> madeDifficultyDfNoTrack free lWholeTask ld'
                               Paragliding -> const $ DifficultyFraction 0.0

                        xs' =
                            (\Cmp.DfNoTrackPilot{pilot = p, awardedReach = aw} -> (p, f aw))
                            <$> xs
                    in
                        (fmap . fmap)
                        (applyDifficulty ps')
                        xs'
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | DfNoTrack xs <- dfNtss
            | ld <- landoutDifficulty
            | lWholeTask <- lsWholeTask
            ]

        nighDistancePointsDf :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\ps' -> (fmap . fmap) (applyLinear free bd ps') ds)
                ps
            | bd <- bestDistance
            | ps <- (fmap . fmap) points allocs
            | ds <- nighDistanceDf
            ]

        nighDistancePointsDfNoTrack :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\ps' -> (fmap . fmap) (applyLinear free bd ps') ds)
                ps
            | bd <- bestDistance
            | ps <- (fmap . fmap) points allocs
            | ds <- nighDistanceDfNoTrack
            ]

        leadingPoints :: [[(Pilot, LeadingPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) (const $ LeadingFraction 0) xs
                        ys' = (fmap . fmap) leadingFraction ys
                    in
                        (fmap . fmap)
                        (applyLeading ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- leadRank
            ]

        arrivalPoints :: [[(Pilot, ArrivalPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) (const $ ArrivalFraction 0) xs
                        ys' = (fmap . fmap) arrivalFraction ys
                    in
                        (fmap . fmap)
                        (applyArrival ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- arrivalRank
            ]

        timePoints :: _ -> [[(Pilot, TimePoints)]] =
            \speed ->
                [ maybe
                    []
                    (\ps' ->
                        let xs' = (fmap . fmap) (const $ SpeedFraction 0) xs
                            ys' = (fmap . fmap) speedFraction ys
                        in
                            (fmap . fmap)
                            (applyTime ps')
                            (xs' ++ ys')
                    )
                    ps
                | ps <- (fmap . fmap) points allocs
                | xs <- nigh
                | ys <- speed
                ]

        elapsedTime :: _ -> [[(Pilot, Maybe (PilotTime (Quantity Double [u| h |])))]] =
            \speed ->
                [ let xs' = (fmap . fmap) (const Nothing) xs
                      ys' = (fmap . fmap) (Just . Speed.time) ys
                  in (xs' ++ ys')
                | xs <- nigh
                | ys <- speed
                ]

        speedSections :: [SpeedSection] = speedSection <$> tasks

        tags :: [[(Pilot, Maybe StartEndTags)]] =
            [ (fmap . fmap) (startEnd . section ss)
              . (\(PilotTrackTag p tag) -> (p, zonesTag <$> tag))
              <$> ts
            | ss <- speedSections
            | ts <- tagging
            ]

        scoreDf :: [[(Pilot, Breakdown)]] =
            [ let dsL = Map.fromList dsLand
                  dsN = Map.fromList dsNigh
                  dsS = Map.fromList dsSpeed
                  ds =
                      Map.toList
                      $ Map.intersectionWith (\a (b, c) -> (a, b, c)) dsS
                      $ Map.intersectionWith (,) dsN dsL
              in
                  rankByTotal . sortScores
                  $ fmap (tallyDf gates)
                  A.<$> collateDf diffs linears ls as ts penals alts ds ssEs gsEs gs
            | diffs <- difficultyDistancePointsDf
            | linears <- nighDistancePointsDf
            | ls <- leadingPoints
            | as <- arrivalPoints
            | ts <- timePoints gsSpeed
            | dsSpeed <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    speedDistance
            | dsNigh <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    nighDistanceDf
            | dsLand <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    landDistance
            | alts <- stoppedAlts
            | ssEs <- elapsedTime ssSpeed
            | gsEs <- elapsedTime gsSpeed
            | gs <- tags
            | gates <- startGates <$> tasks
            | penals <- penals <$> tasks
            ]

        scoreDfNoTrack :: [[(Pilot, Breakdown)]] =
            [ rankByTotal . sortScores
              $ fmap (tallyDfNoTrack gates lSpeedTask lWholeTask)
              A.<$> collateDfNoTrack diffs linears as ts penals dsAward
            | diffs <- difficultyDistancePointsDfNoTrack
            | linears <- nighDistancePointsDfNoTrack
            | as <- arrivalPoints
            | ts <- timePoints gsSpeed
            | dsAward <- dfNtss
            | lSpeedTask <- lsSpeedTask
            | lWholeTask <- lsWholeTask
            | gates <- startGates <$> tasks
            | penals <- penals <$> tasks
            ]

        score :: [[(Pilot, Breakdown)]] =
            [ rankByTotal . sortScores $ xs ++ ys
            | xs <- scoreDf
            | ys <- scoreDfNoTrack
            ]

sortScores :: [(Pilot, Breakdown)] -> [(Pilot, Breakdown)]
sortScores =
    sortBy
        (\(_, Breakdown{total = a}) (_, Breakdown{total = b}) ->
            b `compare` a)

zeroPoints :: Gap.Points
zeroPoints =
    Gap.Points 
        { reach = LinearPoints 0
        , effort = DifficultyPoints 0
        , distance = DistancePoints 0
        , leading = LeadingPoints 0
        , arrival = ArrivalPoints 0
        , time = TimePoints 0
        }

mapOfDifficulty :: Maybe [ChunkDifficulty] -> Map IxChunk DifficultyFraction
mapOfDifficulty Nothing = Map.fromList []
mapOfDifficulty (Just xs) =
    Map.fromList $ (\ChunkDifficulty{chunk, frac} -> (chunk, frac)) <$> xs

applyDifficulty
    :: Gap.Points
    -> DifficultyFraction
    -> DifficultyPoints
applyDifficulty Gap.Points{distance = DistancePoints y} (DifficultyFraction frac) =
    -- NOTE: A fraction of distance points, not a fraction of effort points.
    DifficultyPoints $ frac * y

madeDifficultyDf
    :: MinimumDistance (Quantity Double [u| km |])
    -> Map IxChunk DifficultyFraction
    -> TrackDistance Land
    -> DifficultyFraction
madeDifficultyDf md mapIxToFrac td =
    fromMaybe (DifficultyFraction 0) $ Map.lookup ix mapIxToFrac
    where
        pd = PilotDistance . MkQuantity . fromMaybe 0.0 $ madeLand td
        ix = toIxChunk md pd

madeDifficultyDfNoTrack
    :: MinimumDistance (Quantity Double [u| km |])
    -> Maybe (QTaskDistance Double [u| m |])
    -> Map IxChunk DifficultyFraction
    -> Maybe AwardedDistance
    -> DifficultyFraction
madeDifficultyDfNoTrack md@(MinimumDistance dMin) td mapIxToFrac dAward =
    fromMaybe (DifficultyFraction 0) $ Map.lookup ix mapIxToFrac
    where
        pd :: Quantity Double [u| km |]
        pd =
            case (td, dAward) of
                (_, Nothing) -> dMin
                (Nothing, _) -> dMin
                (Just td', Just dAward') -> awardByFrac (Clamp True) td' dAward'

        ix = toIxChunk md (PilotDistance pd)

madeAwarded
    :: MinimumDistance (Quantity Double [u| km |])
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe AwardedDistance
    -> Maybe Double -- ^ The distance made in km
madeAwarded _ (Just td) (Just dAward) = Just . unQuantity $ awardByFrac (Clamp True) td dAward
madeAwarded (MinimumDistance (MkQuantity d)) _ _ = Just d

madeNigh :: TrackDistance Nigh -> Maybe Double
madeNigh TrackDistance{made} = unTaskDistanceAsKm <$> made

madeLand :: TrackDistance Land -> Maybe Double
madeLand TrackDistance{made} = unTaskDistanceAsKm <$> made

applyLinear
    :: MinimumDistance (Quantity Double [u| km |])
    -> Maybe (QTaskDistance Double [u| m |]) -- ^ The best distance
    -> Gap.Points
    -> Maybe Double -- ^ The distance made in km
    -> LinearPoints
applyLinear _ Nothing _ _ = LinearPoints 0
applyLinear _ _ _ Nothing = LinearPoints 0
applyLinear
    (MinimumDistance (MkQuantity dMin))
    (Just (TaskDistance best))
    Gap.Points{reach = LinearPoints y}
    (Just made) =
        if | best' <= 0 -> LinearPoints 0
           | otherwise -> LinearPoints $ frac * y
    where
        frac :: Rational
        frac = toRational (max dMin made) / toRational best'

        MkQuantity best' = convert best :: Quantity Double [u| km |]

leadingFraction :: TrackLead -> LeadingFraction
leadingFraction TrackLead{frac} = frac

applyLeading :: Gap.Points -> LeadingFraction -> LeadingPoints
applyLeading Gap.Points{leading = LeadingPoints y} (LeadingFraction x) =
    LeadingPoints $ x * y

arrivalFraction :: TrackArrival -> ArrivalFraction
arrivalFraction TrackArrival{frac} = frac

applyArrival :: Gap.Points -> ArrivalFraction -> ArrivalPoints
applyArrival Gap.Points{arrival = ArrivalPoints y} (ArrivalFraction x) =
    ArrivalPoints $ x * y

speedFraction :: Speed.TrackSpeed -> SpeedFraction
speedFraction Speed.TrackSpeed{frac} = frac

applyTime :: Gap.Points -> SpeedFraction -> TimePoints
applyTime Gap.Points{time = TimePoints y} (SpeedFraction x) =
    TimePoints $ x * y

collateDf
    :: [(Pilot, DifficultyPoints)]
    -> [(Pilot, LinearPoints)]
    -> [(Pilot, LeadingPoints)]
    -> [(Pilot, ArrivalPoints)]
    -> [(Pilot, TimePoints)]
    -> [(Pilot, [PointPenalty], String)]
    -> [(Pilot, Maybe alt)]
    -> [(Pilot, (Maybe a, Maybe a, Maybe a))]
    -> [(Pilot, Maybe b)]
    -> [(Pilot, Maybe c)]
    -> [(Pilot, Maybe d)]
    -> [
            ( Pilot
            ,
                ( Maybe alt
                ,
                    ( Maybe d
                    ,
                        ( Maybe c
                        ,
                            ( Maybe b
                            ,
                                ( (Maybe a, Maybe a, Maybe a)
                                ,
                                    ( ([PointPenalty], String)
                                    , Gap.Points
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ]
collateDf diffs linears ls as ts penals alts ds ssEs gsEs gs =
    Map.toList
    $ Map.intersectionWith (,) malts
    $ Map.intersectionWith (,) mg
    $ Map.intersectionWith (,) mgsEs
    $ Map.intersectionWith (,) mssEs
    $ Map.intersectionWith (,) md
    $ Map.intersectionWith (,) (mergePenalties md $ Map.fromList penals')
    $ Map.intersectionWith glueLeading ml
    $ Map.unionWith mergeSpeed mdl mta
    where
        mDiff = Map.fromList diffs
        mLinear = Map.fromList linears
        ml = Map.fromList ls
        ma = Map.fromList as
        mt = Map.fromList ts
        malts = Map.fromList alts
        md = Map.fromList ds
        mssEs = Map.fromList ssEs
        mgsEs = Map.fromList gsEs
        mg = Map.fromList gs
        penals' = tuplePenalty <$> penals

        mdl =
            Map.intersectionWith glueDiff mDiff
            $ zeroLinear <$> mLinear

        mta =
            Map.intersectionWith glueTime mt
            $ zeroArrival <$> ma

collateDfNoTrack
    :: [(Pilot, DifficultyPoints)]
    -> [(Pilot, LinearPoints)]
    -> [(Pilot, ArrivalPoints)]
    -> [(Pilot, TimePoints)]
    -> [(Pilot, [PointPenalty], String)]
    -> DfNoTrack
    -> [
            (Pilot
            ,
                ( (Maybe AwardedDistance, AwardedVelocity)
                ,
                    ( ([PointPenalty], String)
                    , Gap.Points
                    )
                )
            )
        ]
collateDfNoTrack diffs linears as ts penals (DfNoTrack ds) =
    Map.toList
    $ Map.intersectionWith (,) md
    $ Map.intersectionWith (,) (mergePenalties md $ Map.fromList penals')
    $ Map.unionWith mergeSpeed mdl mta
    where
        mDiff = Map.fromList diffs
        mLinear = Map.fromList linears
        ma = Map.fromList as
        mt = Map.fromList ts
        penals' = tuplePenalty <$> penals

        md =
            Map.fromList
            $ (\Cmp.DfNoTrackPilot{pilot = p, awardedReach = aw, awardedVelocity = av} ->
                (p, (aw, av)))
            <$> ds

        mdl =
            Map.intersectionWith glueDiff mDiff
            $ zeroLinear <$> mLinear

        mta =
            Map.intersectionWith glueTime mt
            $ zeroArrival <$> ma

tuplePenalty :: (a, b, c) -> (a, (b, c))
tuplePenalty (a, b, c) = (a, (b, c))

-- | Merge maps so that each pilot has a list of penalties, possibly an empty one.
mergePenalties
    :: Map Pilot a
    -> Map Pilot ([PointPenalty], String)
    -> Map Pilot ([PointPenalty], String)
mergePenalties =
    Map.merge
        (Map.mapMissing (\_ _ -> ([], "")))
        (Map.preserveMissing)
        (Map.zipWithMatched (\_ _ y -> y))

mergeSpeed :: Gap.Points -> Gap.Points -> Gap.Points
mergeSpeed g Gap.Points{Gap.time = t, Gap.arrival = a} =
    g{Gap.time = t, Gap.arrival = a}

zeroLinear :: LinearPoints -> Gap.Points
zeroLinear r = zeroPoints {Gap.reach = r}

zeroArrival :: ArrivalPoints -> Gap.Points
zeroArrival a = zeroPoints {Gap.arrival = a}

glueDiff :: DifficultyPoints -> Gap.Points -> Gap.Points
glueDiff
    effort@(DifficultyPoints diff)
    p@Gap.Points {Gap.reach = LinearPoints linear} =
    p
        { Gap.effort = effort
        , Gap.distance = DistancePoints $ diff + linear
        }

glueLeading :: LeadingPoints -> Gap.Points -> Gap.Points
glueLeading l p = p{Gap.leading = l}

glueTime :: TimePoints -> Gap.Points -> Gap.Points
glueTime t p = p {Gap.time = t}

zeroVelocity :: Velocity
zeroVelocity =
    Velocity
        { ss = Nothing
        , gs = Nothing
        , es = Nothing
        , ssElapsed = Nothing
        , gsElapsed = Nothing
        , ssDistance = Nothing
        , ssVelocity = Nothing
        , gsVelocity = Nothing
        }

mkVelocity
    :: PilotDistance (Quantity Double [u| km |])
    -> PilotTime (Quantity Double [u| h |])
    -> PilotVelocity (Quantity Double [u| km / h |])
mkVelocity (PilotDistance d) (PilotTime t) =
    PilotVelocity $ d /: t

startEnd :: RaceSections (Maybe ZoneTag) -> StartEndTags
startEnd RaceSections{race} =
    case (race, reverse race) of
        ([], _) -> StartEnd Nothing Nothing
        (_, []) -> StartEnd Nothing Nothing
        (x : _, y : _) -> StartEnd x y

tallyDf
    :: [StartGate]
    ->
        ( Maybe (QAlt Double [u| m |])
        ,
            ( Maybe StartEndTags
            ,
                ( Maybe (PilotTime (Quantity Double [u| h |]))
                ,
                    ( Maybe (PilotTime (Quantity Double [u| h |]))
                    ,
                        (
                            ( Maybe (PilotDistance (Quantity Double [u| km |]))
                            , Maybe (PilotDistance (Quantity Double [u| km |]))
                            , Maybe (PilotDistance (Quantity Double [u| km |]))
                            )
                        ,
                            ( ([PointPenalty], String)
                            , Gap.Points
                            )
                        )
                    )
                )
            )
        )
    -> Breakdown
tallyDf
    startGates
    ( alt
    ,
        ( g
        ,
            ( gsT
            ,
                ( ssT
                ,
                    ( (dS, dN, dL)
                    ,
                        ( (penalties, penaltyReason)
                        , x@Gap.Points
                            { reach = LinearPoints r
                            , effort = DifficultyPoints dp
                            , leading = LeadingPoints l
                            , arrival = ArrivalPoints a
                            , time = TimePoints tp
                            }
                        )
                    )
                )
            )
        )
    ) =
    Breakdown
        { place = TaskPlacing 0
        , total = applyPointPenalties penalties total
        , penalties = penalties
        , penaltyReason = penaltyReason
        , breakdown = x
        , velocity =
            Just
            $ zeroVelocity
                { ss = ss'
                , gs = gs
                , es = es'
                , ssDistance = dS
                , ssElapsed = ssT
                , gsElapsed = gsT
                , ssVelocity = liftA2 mkVelocity dS ssT
                , gsVelocity = liftA2 mkVelocity dS gsT
                }
        , reachDistance = dN
        , landedDistance = dL
        , stoppedAlt = alt
        }
    where
        gs = snd <$> do
                ss'' <- ss'
                gs' <- nonEmpty startGates
                return $ startGateTaken gs' ss''

        total = TaskPoints $ r + dp + l + a + tp
        ss' = getTagTime unStart
        es' = getTagTime unEnd
        getTagTime accessor =
            ((time :: InterpolatedFix -> _) . inter)
            <$> (accessor =<< g)

tallyDfNoTrack
    :: [StartGate]
    -> Maybe (QTaskDistance Double [u| m |]) -- ^ Speed section distance
    -> Maybe (QTaskDistance Double [u| m |]) -- ^ Whole task distance
    -> ((Maybe AwardedDistance, AwardedVelocity), (([PointPenalty], String), Gap.Points))
    -> Breakdown
tallyDfNoTrack
    startGates
    dS'
    dT'
    ( (aw', AwardedVelocity{ss, es})
    ,
        ( (penalties, penaltyReason)
        , x@Gap.Points
            { reach = LinearPoints r
            , effort = DifficultyPoints dp
            , leading = LeadingPoints l
            , arrival = ArrivalPoints a
            , time = TimePoints tp
            }
        )
    ) =
    Breakdown
        { place = TaskPlacing 0
        , total = applyPointPenalties penalties total
        , penalties = penalties
        , penaltyReason = penaltyReason
        , breakdown = x

        , velocity =
            case (ss, es) of
                (Just ss', Just _) ->
                    let se = StartEnd ss' es
                        ssT = pilotTime [StartGate ss'] se
                        gsT = pilotTime startGates se
                    in
                        Just
                        $ zeroVelocity
                            { ss = ss
                            , gs = gs
                            , es = es
                            , ssDistance = dS
                            , ssElapsed = ssT
                            , gsElapsed = gsT
                            , ssVelocity = liftA2 mkVelocity dS ssT
                            , gsVelocity = liftA2 mkVelocity dS gsT
                            }
                _ -> Nothing

        , reachDistance = dP
        , landedDistance = dP
        , stoppedAlt = Nothing
        }
    where
        gs = snd <$> do
                ss' <- ss
                gs' <- nonEmpty startGates
                return $ startGateTaken gs' ss'

        total = TaskPoints $ r + dp + l + a + tp
        dP = PilotDistance <$> do
                dT <- dT'
                aw <- aw'
                return $ awardByFrac (Clamp False) dT aw

        dS = PilotDistance <$> do
                TaskDistance d <- dS'
                return $ convert d

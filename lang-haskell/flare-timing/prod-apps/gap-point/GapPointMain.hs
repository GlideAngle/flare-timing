{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

import Prelude hiding (max)
import qualified Prelude as Stats (max)
import Data.Refined (assumeProp, refined)
import Data.Ratio ((%))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing)
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
import Control.Monad.Zip (mzip)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)
import "newtype" Control.Newtype (Newtype(..))
import Data.UnitsOfMeasure ((/:), u, convert, unQuantity, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Distance (QTaskDistance, TaskDistance(..), unTaskDistanceAsKm, fromKms)
import Flight.Route (OptimalRoute(..))
import qualified Flight.Comp as Cmp (DfNoTrackPilot(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , CompTaskSettings(..)
    , Comp(..)
    , Nominal(..)
    , Tweak(..)
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
    , EarlyStart(..)
    , compToMaskLead
    , compToLand
    , compToFar
    , compToPoint
    , findCompInput
    , reshape
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Track.Cross
    (InterpolatedFix(..), CompFlying(..), ZoneTag(..), TrackFlyingSection(..))
import Flight.Track.Tag (CompTagging(..), PilotTrackTag(..), TrackTag(..))
import Flight.Track.Stop (effectiveTagging)
import Flight.Track.Distance
    (TrackDistance(..), AwardedDistance(..), Clamp(..), Nigh, Effort, awardByFrac)
import Flight.Track.Time (AwardedVelocity(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Speed (pilotTime, startGateTaken)
import qualified Flight.Track.Speed as Speed (TrackSpeed(..))
import Flight.Track.Mask
    ( CompMaskingArrival(..)
    , CompMaskingEffort(..)
    , MaskingLead(..)
    , CompMaskingReach(..)
    , CompMaskingSpeed(..)
    )
import Flight.Track.Land (Landing(..))
import Flight.Track.Place (rankByTotal)
import Flight.Track.Point
    (Velocity(..), Breakdown(..), Pointing(..), Allocation(..), EssNotGoal(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    ( readCompAndTasks
    , readRoutes
    , readCompFlyTime, readCompTagZone, readCompPegFrame
    , readCompMaskArrival
    , readCompMaskEffort
    , readMaskingLead
    , readCompMaskBonus
    , readCompMaskReach
    , readCompMaskSpeed
    , readLanding
    , readFaring
    , writePointing
    )
import Flight.Mask (RaceSections(..), section)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)
import Flight.Zone.MkZones (Discipline(..), Zones(..))
import Flight.Lookup.Route (routeLength)
import qualified Flight.Lookup as Lookup (compRoutes)
import "flight-gap-allot" Flight.Score
    ( MinimumDistance(..), LaunchToEss(..)
    , NominalTime(..), BestTime(..)
    , PilotsAtEss(..), PilotsPresent(..), PilotsFlying(..), PilotsLanded(..)
    , FlownMax(..)
    , ArrivalFraction(..), SpeedFraction(..)
    , SumOfDistance(..), PilotDistance(..)
    , DifficultyFraction(..), LeadingFraction(..)
    , TaskPlacing(..), PilotVelocity(..), PilotTime(..)
    , unFlownMaxAsKm
    )
import "flight-gap-effort" Flight.Score
    ( IxChunk(..), ChunkDifficulty(..)
    , toIxChunk
    )
import "flight-gap-math" Flight.Score
    ( DistancePoints(..), LinearPoints(..), DifficultyPoints(..)
    , LeadingPoints(..), ArrivalPoints(..), TimePoints(..)
    , JumpedTheGun(..)
    , TooEarlyPoints(..), LaunchToStartPoints(..)
    , SitRep(..)
    , PenaltySeqs, PointsReduced(..)
    , idSeq, addSeq, nullSeqs, toSeqs, exAdd
    , jumpTheGunSitRepHg, jumpTheGunSitRepPg
    , availablePointsHg, availablePointsPg
    , egPenaltyNull
    )
import qualified "flight-gap-math" Flight.Score as GapMath (egPenalty)
import "flight-gap-valid" Flight.Score
    ( Validity(..), ValidityWorking(..)
    , StopValidity(..), StopValidityWorking
    , ReachToggle(..), ReachStats(..)
    , taskValidity, launchValidity, distanceValidity, timeValidity, stopValidity
    )
import "flight-gap-weight" Flight.Score
    ( GoalRatio(..), Lw(..), Aw(..), Rw(..), Ew(..)
    , distanceRatio, distanceWeight, reachWeight, effortWeight
    , leadingWeight, arrivalWeight, timeWeight
    )
import qualified "flight-gap-math" Flight.Score as Gap (Points(..), taskPoints)
import qualified "flight-gap-valid" Flight.Score as Gap (ReachToggle(..), Validity(..))
import qualified "flight-gap-weight" Flight.Score as Gap (Weights(..))
import GapPointOptions (description)

type StartEndTags = StartEnd (Maybe ZoneTag) ZoneTag

availablePoints :: Discipline -> _
availablePoints HangGliding = availablePointsHg
availablePoints Paragliding = availablePointsPg

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o@CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    Fmt.fprint ("Tallying points completed in " Fmt.% timeSpecs Fmt.% "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    let maskLeadFile = compToMaskLead compFile
    let landFile = compToLand compFile
    let farFile = compToFar compFile
    let pointFile = compToPoint compFile
    putStrLn $ "Reading pilots ABS & DNF from task from " ++ show compFile
    putStrLn $ "Reading leading from " ++ show maskLeadFile
    putStrLn $ "Reading distance difficulty from " ++ show landFile

    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    fys <-
        catchIO
            (Just <$> readCompFlyTime compFile)
            (const $ return Nothing)

    tgs <-
        catchIO
            (Just <$> readCompTagZone compFile)
            (const $ return Nothing)

    stps <-
        catchIO
            (Just <$> readCompPegFrame compFile)
            (const $ return Nothing)

    ma <-
        catchIO
            (Just <$> readCompMaskArrival compFile)
            (const $ return Nothing)

    me <-
        catchIO
            (Just <$> readCompMaskEffort compFile)
            (const $ return Nothing)

    ml2 :: Maybe (MaskingLead [u| (km^2)*s |] [u| 1/((km^2)*s) |]) <-
        catchIO
            (Just <$> (readMaskingLead maskLeadFile))
            (const $ return Nothing)

    mr <-
        catchIO
            (Just <$> readCompMaskReach compFile)
            (const $ return Nothing)

    br <-
        catchIO
            (Just <$> readCompMaskBonus compFile)
            (const $ return Nothing)

    ms <-
        catchIO
            (Just <$> readCompMaskSpeed compFile)
            (const $ return Nothing)

    _landing <-
        catchIO
            (Just <$> readLanding landFile)
            (const $ return Nothing)

    landing <-
        catchIO
            (Just <$> readFaring farFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoutes compFile)
            (const $ return Nothing)

    let lookupTaskLength =
            routeLength
                taskRoute
                taskRouteSpeedSubset
                stopRoute
                startRoute
                routes

    case (filesTaskAndSettings, fys, tgs, stps, ma, me, ml2, mr, br, ms, landing, routes) of
        (Nothing, _, _, _, _, _, _, _, _, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _, _, _, _, _, _, _, _, _) -> putStrLn "Couldn't read the flying times."
        (_, _, Nothing, _, _, _, _, _, _, _, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, _, Nothing, _, _, _, _, _, _, _, _) -> putStrLn "Couldn't read the scored frames."
        (_, _, _, _, Nothing, _, _, _, _, _, _, _) -> putStrLn "Couldn't read the masking arrivals."
        (_, _, _, _, _, Nothing, _, _, _, _, _, _) -> putStrLn "Couldn't read the masking effort."
        (_, _, _, _, _, _, Nothing, _, _, _, _, _) -> putStrLn "Couldn't read the masking leading."
        (_, _, _, _, _, _, _, Nothing, _, _, _, _) -> putStrLn "Couldn't read the masking reach."
        (_, _, _, _, _, _, _, _, Nothing, _, _, _) -> putStrLn "Couldn't read the bonus reach."
        (_, _, _, _, _, _, _, _, _, Nothing, _, _) -> putStrLn "Couldn't read the masking speed."
        (_, _, _, _, _, _, _, _, _, _, Nothing, _) -> putStrLn "Couldn't read the land outs."
        (_, _, _, _, _, _, _, _, _, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just (_taskFiles, settings), Just cg, Just tg, Just stp, Just mA, Just _mE, Just mL2, Just mR, Just bR, Just mS, Just lg, Just _) -> do
            let cs = uncurry mkCompTaskSettings $ settings
            let tg' = effectiveTagging tg stp
            let mE' = efforts lg

            writePointing pointFile $ points' cs lookupTaskLength cg tg' mA mE' mL2 (mR, bR) mS lg

efforts :: Cmp.Landing -> CompMaskingEffort
efforts Cmp.Landing{bestDistance = ds, difficulty = ess} =
    CompMaskingEffort
        { bestEffort = [ do FlownMax d' <- d; return $ fromKms d' | d <- ds ]
        , land = downPilots <$> ess
        }

downPilots
    :: Maybe [ChunkDifficulty]
    -> [(Pilot, TrackDistance Effort)]
downPilots Nothing = []
downPilots (Just xs) =
    concat
    [ zip downers ((\(PilotDistance d) -> toBothWays $ fromKms d) <$> downs)
    | ChunkDifficulty{downers, downs} <- xs
    ]

toBothWays :: QTaskDistance Double [u| m |] -> TrackDistance (QTaskDistance Double [u| m |])
toBothWays d =
    TrackDistance
        { togo = Nothing -- NOTE: Don't care about togo right now.
        , made = Just d
        }

points'
    :: CompTaskSettings k
    -> RoutesLookupTaskDistance
    -> CompFlying
    -> CompTagging
    -> CompMaskingArrival
    -> CompMaskingEffort
    -> MaskingLead _ _
    -> (CompMaskingReach, CompMaskingReach)
    -> CompMaskingSpeed
    -> Cmp.Landing
    -> Pointing
points'
    CompTaskSettings
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
    CompFlying{flying}
    CompTagging{tagging}
    CompMaskingArrival
        { pilotsAtEss
        , arrivalRank
        }
    CompMaskingEffort
        { bestEffort
        , land
        }
    MaskingLead
        { sumDistance
        , leadRank
        }
    ( CompMaskingReach
        { reach = reachStatsF
        , bolster = bolsterStatsF
        , nigh = nighF
        }
    , CompMaskingReach
        { reach = reachStatsE
        , bolster = bolsterStatsE
        , nigh = nighE
        }
    )
    CompMaskingSpeed
        { ssBestTime
        , gsBestTime
        , taskSpeedDistance
        , ssSpeed
        , gsSpeed
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

        lsLaunchToSssTask :: [Maybe (QTaskDistance Double [u| m |])] =
            (fmap . fmap) launchToSssDistance lsTask'

        -- NOTE: If there is no best distance, then either the task wasn't run
        -- or it has not been scored yet.
        maybeTasks :: [a -> Maybe a]
        maybeTasks =
            [ if b == [u| 0 km |] then const Nothing else Just
            | b <- maybe zero (\ReachStats{max = FlownMax d} -> d) <$> bolsterStatsF
            ]

        lvs =
            [ launchValidity
                lNom
                (PilotsPresent . fromInteger $ dfs + dnfs)
                (PilotsFlying . fromInteger $ dfs)
            | dfs <- dfss
            | dnfs <- dnfss
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
                (ReachToggle{extra = bE, flown = bF})
                s
            | pf <- PilotsFlying <$> dfss
            | bE <- maybe (FlownMax zero) (\ReachStats{max = d} -> d) <$> bolsterStatsE
            | bF <- maybe (FlownMax zero) (\ReachStats{max = d} -> d) <$> bolsterStatsF
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
                    (ReachToggle{extra = bE, flown = bF})

                | ssT <- f ssBestTime
                | gsT <- f gsBestTime
                | bE <- maybe (FlownMax zero) (\ReachStats{max = d} -> d) <$> bolsterStatsE
                | bF <- maybe (FlownMax zero) (\ReachStats{max = d} -> d) <$> bolsterStatsF
                ]

        workings :: [Maybe ValidityWorking] =
            [ do
                lv' <- lv
                dv' <- dv
                (flip (ValidityWorking lv' dv') sv) <$> tv
            | lv <- snd <$> lvs
            | dv <- snd <$> dvs
            | tv <- snd <$> tvs
            | sv <- (join . fmap snd) <$> svs
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
            | bd <- maybe zero (\ReachStats{max = FlownMax d} -> d) <$> bolsterStatsE
            | td <- maybe [u| 0.0 km |] (MkQuantity . unTaskDistanceAsKm) <$> lsWholeTask
            ]

        aws =
            [
                arrivalWeight $
                maybe
                    AwZero
                    (\Tweak{arrivalRank = byRank, arrivalTime = byTime} ->
                        if | discipline == Paragliding -> AwZero
                           | byTime -> AwHgTime dw
                           | byRank -> AwHgRank dw
                           | otherwise -> AwZero)
                   tw

            | dw <- dws
            | tw <- taskTweak <$> tasks
            ]

        ws =
            [ Gap.Weights rw ew dw lw aw (timeWeight dw lw aw)
            | dw <- dws
            | rw <- rws
            | ew <- ews
            | lw <- lws
            | aw <- aws
            ]

        -- NOTE: Limited to the pilots we have landing times for.
        plss :: [[(Pilot, UTCTime)]] =
            [
                catMaybes
                [ sequence (p, join $ (fmap snd . flyingTimes) <$> tfs)
                | (p, tfs) <- fts
                ]
            | fts <- flying
            ]

        pls :: [([(Pilot, UTCTime)], [(Pilot, UTCTime)])] =
            [
                case stopped of
                    Nothing -> ([], [])
                    Just TaskStop{retroactive = t} ->
                        partition
                            ((< t) . snd)
                            pfs

            | pfs <- plss
            | Task{stopped} <- tasks
            ]

        svs :: [Maybe (StopValidity, Maybe StopValidityWorking)] =
            [
                do
                    _ <- sp
                    ed' <- ed

                    let ls = PilotsLanded . fromIntegral . length $ snd <$> landedByStop
                    let sf = PilotsFlying . fromIntegral . length $ snd <$> stillFlying
                    let r = ReachToggle{extra = rE, flown = rF}

                    return $ stopValidity pf pe ls sf r ed'

            | sp <- stopped <$> tasks
            | pf <- PilotsFlying <$> dfss
            | pe <- pilotsAtEss
            | (landedByStop, stillFlying) <- pls

            | rE <- reachStatsE
            | rF <- reachStatsF

            | ed <-
                (fmap . fmap)
                    (\(TaskDistance td) -> LaunchToEss $ convert td)
                    lsLaunchToEssTask
            ]

        validities :: [Maybe Validity] =
            [ maybeTask $ Validity (taskValidity lv dv tv sv) lv dv tv sv
            | lv <- fst <$> lvs
            | dv <- fst <$> dvs
            | tv <- fst <$> tvs
            | sv <- fmap fst <$> svs
            | maybeTask <- maybeTasks
            ]

        allocs :: [Maybe Allocation]=
            [ do
                v' <- v
                let (pts, taskPoints) = availablePoints discipline v' w
                return $ Allocation gr w pts taskPoints
            | gr <- grs
            | w <- ws
            | v <- (fmap . fmap) Gap.task validities
            ]

        -- NOTE: Pilots either get to goal or have a nigh distance.
        nighDistanceDfE :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeNigh xs
                  f = (\ReachStats{max = b} -> unFlownMaxAsKm b) <$> rStats
                  ys' = (fmap . fmap) (const f) ys
              in (xs' ++ ys')
            | rStats <- bolsterStatsE
            | xs <- nighE
            | ys <- arrivalRank
            ]

        nighDistanceDfF :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeNigh xs
                  f = (\ReachStats{max = b} -> unFlownMaxAsKm b) <$> rStats
                  ys' = (fmap . fmap) (const f) ys
              in (xs' ++ ys')
            | rStats <- bolsterStatsF
            | xs <- nighF
            | ys <- arrivalRank
            ]

        nighDistanceDfNoTrackE :: [[(Pilot, Maybe Double)]] =
            [
                (\Cmp.DfNoTrackPilot{pilot = p, awardedReach = aw} ->
                    (p, madeAwarded free lWholeTask $ Gap.extra <$> aw))
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
            | xs <- nighF
            | ys <- arrivalRank
            ]

        -- NOTE: Pilots either get to goal or have a landing distance.
        landDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeLand xs
                  ys' = (fmap . fmap) (const bd) ys
              in (xs' ++ ys')
            | bd <- (fmap . fmap) unTaskDistanceAsKm bestEffort
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

        dFree = TaskDistance . convert $ unpack free

        difficultyDistancePointsDfNoTrack :: [[(Pilot, DifficultyPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let ld' = mapOfDifficulty ld

                        f = discipline & \case
                               HangGliding -> madeDifficultyDfNoTrack free lWholeTask ld'
                               Paragliding -> const $ DifficultyFraction 0.0

                        -- NOTE: These pilots get at least free distance.
                        freeOrMore x@AwardedDistance{awardedMade = d} =
                            let made = Stats.max dFree d in
                                if | made == d -> x
                                   | otherwise ->
                                        maybe
                                            x
                                            (\(TaskDistance (MkQuantity td)) ->
                                                let (TaskDistance (MkQuantity df)) = made in
                                                x
                                                    { awardedMade = made
                                                    , awardedFrac = min 1 $ df / td
                                                    })
                                            lWholeTask

                        xs' =
                            (\Cmp.DfNoTrackPilot{pilot = p, awardedReach = aw} ->
                                (p, f . fmap freeOrMore $ Gap.extra <$> aw))
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

        tooEarlyPoints :: [TooEarlyPoints]
        tooEarlyPoints =
            [
                TooEarlyPoints . assumeProp . refined . round . unpack
                $ maybe
                    (LinearPoints 0)
                    (\(ReachStats{max = FlownMax b}, ps') ->
                        let bd = Just . TaskDistance $ convert b in
                        (applyLinear free bd ps') (Just . unQuantity $ unpack free))
                    (mzip rStats ps)
            | rStats <- bolsterStatsE
            | ps <- (fmap . fmap) points allocs
            ]

        launchToStartPoints :: [LaunchToStartPoints]
        launchToStartPoints =
            [
                LaunchToStartPoints . assumeProp . refined . round . unpack
                $ maybe
                    (LinearPoints 0)
                    (\(ReachStats{max = FlownMax b}, ps') ->
                        let bd = Just . TaskDistance $ convert b
                        in (applyLinear free bd ps') launchToStart)
                    (mzip rStats ps)
            | rStats <- bolsterStatsE
            | ps <- (fmap . fmap) points allocs
            | launchToStart <-
                (fmap . fmap)
                    (\(TaskDistance td) ->
                        let ss :: Quantity _ [u| km |]
                            ss = convert td
                         in unQuantity ss)
                    lsLaunchToSssTask
            ]

        nighDistancePointsDfE :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\(ReachStats{max = FlownMax b}, ps') ->
                    let bd = Just . TaskDistance $ convert b in
                    (fmap . fmap) (applyLinear free bd ps') ds)
                (mzip rStats ps)
            | rStats <- bolsterStatsE
            | ps <- (fmap . fmap) points allocs
            | ds <- nighDistanceDfE
            ]

        nighDistancePointsDfNoTrackE :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\(ReachStats{max = FlownMax b}, ps') ->
                    let bd = Just . TaskDistance $ convert b in
                    (fmap . fmap) (applyLinear free bd ps') ds)
                (mzip rStats ps)
            | rStats <- bolsterStatsE
            | ps <- (fmap . fmap) points allocs
            | ds <- nighDistanceDfNoTrackE
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
            | xs <- nighF
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
            | xs <- nighF
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
                | xs <- nighF
                | ys <- speed
                ]

        elapsedTime :: _ -> [[(Pilot, Maybe (PilotTime (Quantity Double [u| h |])))]] =
            \speed ->
                [ let xs' = (fmap . fmap) (const Nothing) xs
                      ys' = (fmap . fmap) (Just . Speed.time) ys
                  in (xs' ++ ys')
                | xs <- nighF
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

        essNotGoals :: [[(Pilot, Maybe EssNotGoal)]] =
            [
                [ (p,) $ do
                    zs <- zonesTag <$> tag
                    startToEss@(_, e) <- ss

                    let essToGoal = (e, lenZs)
                    let madeE = all isJust $ sliceZones (Just startToEss) zs
                    let missedG = any isNothing $ sliceZones (Just essToGoal) zs

                    return . EssNotGoal $ madeE && missedG

                | PilotTrackTag p tag <- ts
                ]
            | Task{zones = Zones{raw = zsRaw}} <- tasks
            , let lenZs = length zsRaw
            | ss <- speedSections
            | ts <- tagging
            ]

        scoreDf :: [[(Pilot, Breakdown)]] =
            [ let dsL = Map.fromList dsLand
                  dsE = Map.fromList dsNighE
                  dsF = Map.fromList dsNighF
                  dsS = Map.fromList dsSpeed
                  ds =
                      Map.toList
                      $ Map.intersectionWith (\s (e, f, l) -> (s, e, f, l)) dsS
                      $ Map.intersectionWith (\e (f, l) -> (e, f, l)) dsE
                      $ Map.intersectionWith (,) dsF dsL

              in
                  rankByTotal . sortScores
                  $ fmap (tallyDf discipline egPenalty startGates hgTooE pgTooE earlyStart)
                  A.<$> collateDf diffs linears ls as ts penals alts ds ssEs gsEs egs gs
            | hgTooE <- tooEarlyPoints
            | pgTooE <- launchToStartPoints
            | diffs <- difficultyDistancePointsDf
            | linears <- nighDistancePointsDfE
            | ls <- leadingPoints
            | as <- arrivalPoints
            | ts <- timePoints gsSpeed
            | dsSpeed <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    speedDistance
            | dsNighE <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    nighDistanceDfE
            | dsNighF <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    nighDistanceDfF
            | dsLand <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    landDistance
            | alts <- stoppedAlts
            | ssEs <- elapsedTime ssSpeed
            | gsEs <- elapsedTime gsSpeed
            | gs <- tags
            | Task{startGates, earlyStart, taskTweak} <- tasks
            , let egPenalty =
                      maybe
                          egPenaltyNull
                          (\Tweak{essNotGoalScaling = x} -> GapMath.egPenalty x)
                          taskTweak
            | penals <- penals <$> tasks
            | egs <- essNotGoals
            ]

        scoreDfNoTrack :: [[(Pilot, Breakdown)]] =
            [ rankByTotal . sortScores
              $ fmap (tallyDfNoTrack discipline gates lSpeedTask lWholeTask)
              A.<$> collateDfNoTrack diffs linears as ts penals dsAward
            | diffs <- difficultyDistancePointsDfNoTrack
            | linears <- nighDistancePointsDfNoTrackE
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
    DifficultyPoints $ realToFrac frac * y

madeDifficultyDf
    :: MinimumDistance (Quantity Double [u| km |])
    -> Map IxChunk DifficultyFraction
    -> TrackDistance Effort
    -> DifficultyFraction
madeDifficultyDf _ mapIxToFrac td =
    -- WARNING: The pilot distance, if rounded up could index the next chunk.
    case Map.lookup ix mapIxToFrac of
        Just df -> df
        Nothing ->
            case Map.lookupLE ix mapIxToFrac of
                Just (ixLE, df) | ix - ixLE == 1 -> df
                Just _ -> DifficultyFraction 0
                Nothing -> DifficultyFraction 0
    where
        pd = PilotDistance . MkQuantity . fromMaybe 0.0 $ madeLand td
        ix = toIxChunk pd

madeDifficultyDfNoTrack
    :: MinimumDistance (Quantity Double [u| km |])
    -> Maybe (QTaskDistance Double [u| m |])
    -> Map IxChunk DifficultyFraction
    -> Maybe AwardedDistance
    -> DifficultyFraction
madeDifficultyDfNoTrack (MinimumDistance dMin) td mapIxToFrac dAward =
    fromMaybe (DifficultyFraction 0) $ Map.lookup ix mapIxToFrac
    where
        pd :: Quantity Double [u| km |]
        pd =
            case (td, dAward) of
                (_, Nothing) -> dMin
                (Nothing, _) -> dMin
                (Just td', Just dAward') ->
                    -- WARNING: Don't allow awardedFrac to give a pilot
                    -- distance less than the free distance.
                    Stats.max dMin $ awardByFrac (Clamp True) td' dAward'

        ix = toIxChunk (PilotDistance pd)

madeAwarded
    :: MinimumDistance (Quantity Double [u| km |])
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe AwardedDistance
    -> Maybe Double -- ^ The distance made in km
madeAwarded _ (Just td) (Just dAward) = Just . unQuantity $ awardByFrac (Clamp True) td dAward
madeAwarded (MinimumDistance (MkQuantity d)) _ _ = Just d

madeNigh :: TrackDistance Nigh -> Maybe Double
madeNigh TrackDistance{made} = unTaskDistanceAsKm <$> made

madeLand :: TrackDistance Effort -> Maybe Double
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
        frac = toRational (Stats.max dMin made) / toRational best'

        MkQuantity best' = convert best :: Quantity Double [u| km |]

leadingFraction :: TrackLead u -> LeadingFraction
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
    -> [(Pilot, PenaltySeqs, String)]
    -> [(Pilot, Maybe alt)]
    -> [(Pilot, (Maybe a, Maybe a, Maybe a, Maybe a))]
    -> [(Pilot, Maybe b)]
    -> [(Pilot, Maybe c)]
    -> [(Pilot, Maybe EssNotGoal)]
    -> [(Pilot, Maybe d)]
    -> [
            ( Pilot
            ,
                ( Maybe alt
                ,
                    ( Maybe d
                    ,
                        ( Maybe EssNotGoal
                        ,
                            ( Maybe c
                            ,
                                ( Maybe b
                                ,
                                    ( (Maybe a, Maybe a, Maybe a, Maybe a)
                                    ,
                                        ( (PenaltySeqs, String)
                                        , Gap.Points
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ]

collateDf diffs linears ls as ts penals alts ds ssEs gsEs egs gs =
    Map.toList
    $ Map.intersectionWith (,) malts
    $ Map.intersectionWith (,) mg
    $ Map.intersectionWith (,) meg
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
        meg = Map.fromList egs
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
    -> [(Pilot, PenaltySeqs, String)]
    -> DfNoTrack
    -> [
            (Pilot
            ,
                ( (Maybe (ReachToggle AwardedDistance), AwardedVelocity)
                ,
                    ( (PenaltySeqs, String)
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
    -> Map Pilot (PenaltySeqs, String)
    -> Map Pilot (PenaltySeqs, String)
mergePenalties =
    Map.merge
        (Map.mapMissing (\_ _ -> (nullSeqs, "")))
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
    :: Discipline
    -> _
    -> [StartGate]
    -> TooEarlyPoints
    -> LaunchToStartPoints
    -> EarlyStart
    ->
        ( Maybe (QAlt Double [u| m |])
        ,
            ( Maybe StartEndTags
            ,
                ( Maybe EssNotGoal
                ,
                    ( Maybe (PilotTime (Quantity Double [u| h |]))
                    ,
                        ( Maybe (PilotTime (Quantity Double [u| h |]))
                        ,
                            (
                                ( Maybe (PilotDistance (Quantity Double [u| km |]))
                                , Maybe (PilotDistance (Quantity Double [u| km |]))
                                , Maybe (PilotDistance (Quantity Double [u| km |]))
                                , Maybe (PilotDistance (Quantity Double [u| km |]))
                                )
                            ,
                                ( (PenaltySeqs, String)
                                , Gap.Points
                                )
                            )
                        )
                    )
                )
            )
        )
    -> Breakdown
tallyDf
    hgOrPg
    egPenalty
    startGates
    tooEarlyPoints
    launchToStartPoints
    EarlyStart{earliest, earlyPenalty = spp}
    ( alt
    ,
        ( g
        ,
            ( eg
            ,
            ( gsT
            ,
                ( ssT
                ,
                    ( (dS, dE, dF, dL)
                    ,
                        ((penalties, penaltyReason), x)
                    )
                )
            )
            )
        )
    ) =
    Breakdown
        { place = TaskPlacing 0
        , subtotal = subtotal
        , demeritFrac = mulApplied
        , demeritPoint = addApplied
        , demeritReset = resetApplied
        , total = total
        , essNotGoal = eg
        , penaltiesEssNotGoal = toSeqs effg
        , jump = jump
        , penaltiesJumpRaw = jRaw
        , penaltiesJumpEffective = jEffective
        , penalties = toSeqs effp
        , penaltyReason = penaltyReason
        , breakdown = x
        , velocity =
            Just
            $ zeroVelocity
                { ss = ss'
                , gs = snd <$> jumpGate
                , es = es'
                , ssDistance = dS
                , ssElapsed = ssT
                , gsElapsed = gsT
                , ssVelocity = liftA2 mkVelocity dS ssT
                , gsVelocity = liftA2 mkVelocity dS gsT
                }

        , reach = do
            dE' <- dE
            dF' <- dF
            return ReachToggle{extra = dE', flown = dF'}

        , landedMade = dL
        , stoppedAlt = alt
        }
    where
        jumpGate :: Maybe (Maybe (JumpedTheGun _), StartGate)
        jumpGate = do
                ss'' <- ss'
                gs' <- nonEmpty startGates
                return $ startGateTaken gs' ss''

        jump :: Maybe (JumpedTheGun _)
        jump = join $ fst <$> jumpGate

        ptsReduced =
            case hgOrPg of
                -- TODO: Workout what the sitrep is here. It won't always be
                -- NominalHg.
                HangGliding ->
                    fromMaybe
                        (let sitrep =
                                 case eg of
                                     Nothing -> NominalHg
                                     Just (EssNotGoal False) -> NominalHg
                                     Just (EssNotGoal True) -> NoGoalHg
                         in
                             Gap.taskPoints sitrep egPenalty idSeq penalties x) $
                    do
                        jtg@(JumpedTheGun jSecs) <- jump
                        return $
                            case jumpTheGunSitRepHg tooEarlyPoints earliest spp jtg of
                                Left j ->
                                    Gap.taskPoints
                                        (Jumped tooEarlyPoints spp (JumpedTheGun jSecs))
                                        egPenalty
                                        (addSeq $ exAdd j)
                                        penalties
                                        x

                                Right sitrep ->
                                    Gap.taskPoints sitrep egPenalty idSeq penalties x

                Paragliding ->
                    fromMaybe
                        (let sitrep =
                                 case eg of
                                     Nothing -> NominalPg
                                     Just (EssNotGoal False) -> NominalPg
                                     Just (EssNotGoal True) -> NoGoalPg
                         in
                             Gap.taskPoints sitrep egPenalty idSeq penalties x) $ do
                    jtg <- jump
                    sitrep <- jumpTheGunSitRepPg launchToStartPoints jtg
                    return $ Gap.taskPoints sitrep egPenalty idSeq penalties x

        ptsReduced' =
            case ptsReduced of
                Left e -> error $ show e
                Right y -> y

        PointsReduced
                { subtotal
                , mulApplied
                , addApplied
                , resetApplied
                , total
                , effp
                , effj
                , rawj
                , effg
                } = ptsReduced'

        jEffective = toSeqs effj
        jRaw = if rawj /= jEffective then Just rawj else Nothing

        ss' = getTagTime unStart
        es' = getTagTime unEnd
        getTagTime accessor =
            ((time :: InterpolatedFix -> _) . inter)
            <$> (accessor =<< g)

tallyDfNoTrack
    :: Discipline
    -> [StartGate]
    -> Maybe (QTaskDistance Double [u| m |]) -- ^ Speed section distance
    -> Maybe (QTaskDistance Double [u| m |]) -- ^ Whole task distance
    ->
        (
            ( Maybe (ReachToggle AwardedDistance)
            , AwardedVelocity
            )
        ,
            ( (PenaltySeqs, String)
            , Gap.Points
            )
        )
    -> Breakdown
tallyDfNoTrack
    hgOrPg
    startGates
    dS'
    dT'
    ( (aw', AwardedVelocity{ss, es})
    ,
        ((penalties, penaltyReason), x)
    ) =
    Breakdown
        { place = TaskPlacing 0
        , subtotal = subtotal
        , demeritFrac = mulApplied
        , demeritPoint = addApplied
        , demeritReset = resetApplied
        , total = total
        , essNotGoal = Nothing
        , penaltiesEssNotGoal = nullSeqs
        , jump = Nothing
        , penaltiesJumpRaw = Nothing
        , penaltiesJumpEffective = nullSeqs
        , penalties = toSeqs effp
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

        , reach = do
            dE' <- dE
            dF' <- dF
            return $ ReachToggle{extra = dE', flown = dF'}

        , landedMade = dE
        , stoppedAlt = Nothing
        }
    where
        gs = snd <$> do
                ss' <- ss
                gs' <- nonEmpty startGates
                return $ startGateTaken gs' ss'

        -- WARNING: Irrefutible pattern, allowing error if unmatched.
        Right
            PointsReduced
                { subtotal
                , mulApplied
                , addApplied
                , resetApplied
                , total
                , effp
                } =
                    case hgOrPg of
                        HangGliding -> Gap.taskPoints NominalHg egPenaltyNull idSeq penalties x
                        Paragliding -> Gap.taskPoints NominalPg egPenaltyNull idSeq penalties x

        dE = PilotDistance <$> do
                dT <- dT'
                aw <- aw'
                return $ awardByFrac (Clamp False) dT (Gap.extra aw)

        dF = PilotDistance <$> do
                dT <- dT'
                aw <- aw'
                return $ awardByFrac (Clamp False) dT (Gap.flown aw)

        dS = PilotDistance <$> do
                TaskDistance d <- dS'
                return $ convert d

{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Data.Function (on)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, groupBy)
import Control.Applicative (liftA2)
import qualified Control.Applicative as A ((<$>))
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)
import Data.Yaml (prettyPrintParseException)
import Data.UnitsOfMeasure ((/:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Distance (QTaskDistance, TaskDistance(..), unTaskDistanceAsKm)
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , Pilot
    , SpeedSection
    , StartGate(..)
    , StartEnd(..)
    , Task(..)
    , compToCross
    , crossToTag
    , compToMask
    , compToLand
    , compToPoint
    , findCompInput
    , ensureExt
    )
import Flight.Track.Cross (Crossing(..), Fix(..))
import Flight.Track.Tag (Tagging(..), PilotTrackTag(..), TrackTag(..))
import Flight.Track.Distance (TrackDistance(..), Nigh, Land)
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Arrival (TrackArrival(..))
import qualified Flight.Track.Speed as Speed (TrackSpeed(..), startGateTaken)
import Flight.Track.Mask (Masking(..))
import Flight.Track.Land (Landing(..))
import Flight.Track.Point
    (Velocity(..), Breakdown(..), Pointing(..), Allocation(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    (readComp, readCrossing, readTagging, readMasking, readLanding, writePointing)
import Flight.Mask (RaceSections(..), section)
import Flight.Zone.MkZones (Discipline(..))
import Flight.Score
    ( MinimumDistance(..), MaximumDistance(..)
    , BestDistance(..), SumOfDistance(..), PilotDistance(..)
    , PilotsAtEss(..), PilotsPresent(..), PilotsFlying(..)
    , GoalRatio(..), Lw(..), Aw(..)
    , NominalTime(..), BestTime(..)
    , Validity(..), ValidityWorking(..)
    , DifficultyFraction(..), LeadingFraction(..)
    , ArrivalFraction(..), SpeedFraction(..)
    , DistancePoints(..), LinearPoints(..), DifficultyPoints(..)
    , LeadingPoints(..), ArrivalPoints(..), TimePoints(..)
    , TaskPlacing(..), TaskPoints(..), PilotVelocity(..), PilotTime(..)
    , IxChunk(..), ChunkDifficulty(..)
    , distanceWeight, leadingWeight, arrivalWeight, timeWeight
    , taskValidity, launchValidity, distanceValidity, timeValidity
    , availablePoints
    , toIxChunk
    )
import qualified Flight.Score as Gap (Validity(..), Points(..), Weights(..))
import GapPointOptions (description)
import Data.Ratio.Rounding (dpRound)

type StartEndTags = StartEnd (Maybe Fix) Fix

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
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag crossFile
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile@(LandOutFile landPath) = compToLand compFile
    let pointFile = compToPoint compFile
    putStrLn $ "Reading pilots absent from task from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading pilots that did not fly from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading start and end zone tagging from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading masked tracks from '" ++ takeFileName maskPath ++ "'"
    putStrLn $ "Reading distance difficulty from '" ++ takeFileName landPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    crossing <- runExceptT $ readCrossing crossFile
    tagging <- runExceptT $ readTagging tagFile
    masking <- runExceptT $ readMasking maskFile
    landing <- runExceptT $ readLanding landFile

    let ppr = putStrLn . prettyPrintParseException

    case (compSettings, crossing, tagging, masking, landing) of
        (Left e, _, _, _, _) -> ppr e
        (_, Left e, _, _, _) -> ppr e
        (_, _, Left e, _, _) -> ppr e
        (_, _, _, Left e, _) -> ppr e
        (_, _, _, _, Left e) -> ppr e
        (Right cs, Right cg, Right tg, Right mk, Right lg) ->
            writePointing pointFile $ points' cs cg tg mk lg

points'
    :: CompSettings k
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
        }
    Crossing{dnf}
    Tagging{tagging}
    Masking
        { pilotsAtEss
        , taskSpeedDistance
        , bestDistance
        , sumDistance
        , ssBestTime
        , gsBestTime
        , lead
        , arrival
        , ssSpeed
        , gsSpeed
        , nigh
        , land
        }
    Landing
        { difficulty = landoutDifficulty
        } =
    Pointing 
        { validityWorking = workings
        , validity = validities
        , allocation = allocs
        , score = score
        }
    where
        -- NOTE: If there is no best distance, then either the task wasn't run
        -- or it has not been scored yet.
        maybeTasks :: [a -> Maybe a]
        maybeTasks =
            [ if null ds then const Nothing else Just | ds <- bestDistance ]

        lvs =
            [ launchValidity
                lNom
                (PilotsPresent . fromInteger $ p)
                (PilotsFlying . fromInteger $ p - d)
            | p <- toInteger . length <$> pilots
            | d <- toInteger . length <$> dnf
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
                (PilotsFlying $ p - d)
                -- TODO: Read minimum distance from the comp.
                (MinimumDistance [u| 5 km |])
                b
                s
            | p <- toInteger . length <$> pilots
            | d <- toInteger . length <$> dnf
            | b <- dBests
            | s <- dSums
            ]

        workings :: [Maybe ValidityWorking] =
            [ do
                lv' <- lv
                dv' <- dv
                ValidityWorking lv' dv' <$> tv
            | lv <- snd <$> lvs
            | dv <- snd <$> dvs
            | tv <- snd <$> tvs
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

        grs =
            [ GoalRatio $ n % toInteger (p - d)
            | n <- (\(PilotsAtEss x) -> x) <$> pilotsAtEss
            | p <- length <$> pilots
            | d <- length <$> dnf
            ]

        dws = distanceWeight <$> grs

        lws =
            let lw = if discipline == HangGliding then LwHg else LwPg
            in leadingWeight . lw <$> dws

        aws =
            if discipline == HangGliding
               then arrivalWeight . AwHg <$> dws
               else const (arrivalWeight AwPg) <$> dws

        ws =
            [ Gap.Weights dw lw aw (timeWeight dw lw aw)
            | dw <- dws
            | lw <- lws
            | aw <- aws
            ]

        validities =
            [ maybeTask $ Validity (taskValidity lv dv tv) lv dv tv
            | lv <- fst <$> lvs
            | dv <- fst <$> dvs
            | tv <- fst <$> tvs
            | maybeTask <- maybeTasks
            ]

        allocs =
            [ uncurry (Allocation gr w) . (`availablePoints` w) <$> v
            | gr <- grs
            | w <- ws
            | v <- (fmap . fmap) Gap.task validities
            ]

        -- NOTE: Pilots either get to goal or have a nigh distance.
        nighDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeNigh xs
                  ys' = (fmap . fmap) (const bd) ys
              in (xs' ++ ys')
            | bd <- (fmap . fmap) unTaskDistanceAsKm bestDistance
            | xs <- nigh
            | ys <- arrival
            ]

        -- NOTE: Pilots either get to the end of the speed section or
        -- they don't and will not get a speed over that section.
        speedDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) (const Nothing) xs
                  ys' = (fmap . fmap) (const sd) ys
              in (xs' ++ ys')
            | sd <- (fmap . fmap) unTaskDistanceAsKm taskSpeedDistance
            | xs <- nigh
            | ys <- arrival
            ]

        -- NOTE: Pilots either get to goal or have a landing distance.
        landDistance :: [[(Pilot, Maybe Double)]] =
            [ let xs' = (fmap . fmap) madeLand xs
                  ys' = (fmap . fmap) (const bd) ys
              in (xs' ++ ys')
            | bd <- (fmap . fmap) unTaskDistanceAsKm bestDistance
            | xs <- land
            | ys <- arrival
            ]

        difficultyDistancePoints :: [[(Pilot, DifficultyPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let ld' = mapOfDifficulty ld
                        xs' = (fmap . fmap) (madeDifficulty free ld') xs
                        ys' = (fmap . fmap) (const $ DifficultyFraction 0.5) ys
                    in
                        (fmap . fmap)
                        (applyDifficulty ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- land
            | ys <- arrival
            | ld <- landoutDifficulty
            ]

        nighDistancePoints :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\ps' ->
                    (fmap . fmap)
                    (applyLinear bd ps')
                    ds
                )
                ps
            | bd <- bestDistance
            | ps <- (fmap . fmap) points allocs
            | ds <- nighDistance
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
            | ys <- lead
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
            | ys <- arrival
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

        score :: [[(Pilot, Breakdown)]] =
            [ let dsL = Map.fromList dsLand
                  dsN = Map.fromList dsNigh
                  dsS = Map.fromList dsSpeed
                  ds =
                      Map.toList
                      $ Map.intersectionWith (\a (b, c) -> (a, b, c)) dsS
                      $ Map.intersectionWith (,) dsN dsL
              in
                  rankByTotal . sortScores
                  $ fmap  (tally gates)
                  A.<$> collate diffs linears ls as ts ds ssEs gsEs gs
            | diffs <- difficultyDistancePoints
            | linears <- nighDistancePoints
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
                    nighDistance
            | dsLand <-
                (fmap . fmap)
                    ((fmap . fmap) (PilotDistance . MkQuantity))
                    landDistance
            | ssEs <- elapsedTime ssSpeed
            | gsEs <- elapsedTime gsSpeed
            | gs <- tags
            | gates <- startGates <$> tasks
            ]

reIndex :: [(Integer, [a])] -> [(Integer, [a])]
reIndex xs =
    zipWith3
        (\i zs o ->
            -- NOTE: Use j so that we get; 1,2=,2=,4 and not 1,3=,3=,4.
            let j = fromIntegral $ length zs - 1
            in (i + (fromIntegral o) - j, zs))
        ixs
        ys
        offsets
    where
        (ixs, ys) = unzip xs
        lens = (\y -> (length y) - 1) <$> ys
        offsets = scanl1 (+) lens

-- SEE: https://stackoverflow.com/questions/51572782/how-to-create-a-ranking-based-on-a-list-of-scores-in-haskell
-- SEE: https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
rankByTotal :: [(Pilot, Breakdown)] -> [(Pilot, Breakdown)]
rankByTotal xs =
    [ (rankScore f ii) <$> y
    | (ii, ys) <-
                reIndex
                . zip [1..]
                . groupBy ((==) `on` truncateTaskPoints . total . snd)
                $ xs
    , let f = if length ys == 1 then TaskPlacing else TaskPlacingEqual
    , y <- ys
    ]

sortScores :: [(Pilot, Breakdown)] -> [(Pilot, Breakdown)]
sortScores =
    sortBy
        (\(_, Breakdown{total = a}) (_, Breakdown{total = b}) ->
            b `compare` a)

truncateTaskPoints :: TaskPoints -> Integer
truncateTaskPoints (TaskPoints x) = truncate . dpRound 0 $ x

rankScore :: (Integer -> TaskPlacing) -> Integer -> Breakdown -> Breakdown
rankScore f ii b = b{place = f ii}

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

madeDifficulty
    :: MinimumDistance (Quantity Double [u| km |])
    -> Map IxChunk DifficultyFraction
    -> TrackDistance Land
    -> DifficultyFraction
madeDifficulty md mapIxToFrac td =
    fromMaybe (DifficultyFraction 0) $ Map.lookup ix mapIxToFrac
    where
        pd = PilotDistance . MkQuantity . fromMaybe 0.0 $ madeLand td
        ix = toIxChunk md pd

madeNigh :: TrackDistance Nigh -> Maybe Double
madeNigh TrackDistance{made} = unTaskDistanceAsKm <$> made

madeLand :: TrackDistance Land -> Maybe Double
madeLand TrackDistance{made} = unTaskDistanceAsKm <$> made

-- TODO: If made < minimum distance, use minimum distance.
applyLinear
    :: Maybe (QTaskDistance Double [u| m |])-- ^ The best distance
    -> Gap.Points
    -> Maybe Double -- ^ The distance made
    -> LinearPoints
applyLinear Nothing _ _ = LinearPoints 0
applyLinear _ _ Nothing = LinearPoints 0
applyLinear
    (Just (TaskDistance best))
    Gap.Points{reach = LinearPoints y}
    (Just made) =
        if | best' <= 0 -> LinearPoints 0
           | otherwise -> LinearPoints $ frac * y
    where
        frac :: Rational
        frac = toRational made / toRational best'

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

collate
    :: [(Pilot, DifficultyPoints)]
    -> [(Pilot, LinearPoints)]
    -> [(Pilot, LeadingPoints)]
    -> [(Pilot, ArrivalPoints)]
    -> [(Pilot, TimePoints)]
    -> [(Pilot, (Maybe a, Maybe a, Maybe a))]
    -> [(Pilot, Maybe b)]
    -> [(Pilot, Maybe c)]
    -> [(Pilot, Maybe d)]
    -> [(Pilot, (Maybe d, (Maybe c, (Maybe b, ((Maybe a, Maybe a, Maybe a), Gap.Points)))))]
collate diffs linears ls as ts ds ssEs gsEs gs =
    Map.toList
    $ Map.intersectionWith (,) mg
    $ Map.intersectionWith (,) mgsEs
    $ Map.intersectionWith (,) mssEs
    $ Map.intersectionWith (,) md
    $ Map.intersectionWith glueDiff mDiff
    $ Map.intersectionWith glueLinear mLinear
    $ Map.intersectionWith glueTime mt
    $ Map.intersectionWith glueLA ml ma
    where
        mDiff = Map.fromList diffs
        mLinear = Map.fromList linears
        ml = Map.fromList ls
        ma = Map.fromList as
        mt = Map.fromList ts
        md = Map.fromList ds
        mssEs = Map.fromList ssEs
        mgsEs = Map.fromList gsEs
        mg = Map.fromList gs

glueDiff :: DifficultyPoints -> Gap.Points -> Gap.Points
glueDiff
    effort@(DifficultyPoints diff)
    p@Gap.Points {Gap.reach = LinearPoints linear} =
    p
        { Gap.effort = effort
        , Gap.distance = DistancePoints $ diff + linear
        }

glueLinear :: LinearPoints -> Gap.Points -> Gap.Points
glueLinear r p = p {Gap.reach = r}

glueLA :: LeadingPoints -> ArrivalPoints -> Gap.Points
glueLA l a = zeroPoints {Gap.leading = l, Gap.arrival = a}

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

tally
    :: [StartGate]
    ->
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
                    , Gap.Points
                    )
                )
            )
        )
    -> Breakdown
tally
    startGates
    ( g
    ,
        ( gsT
        ,
            ( ssT
            ,
                ( (dS, dN, dL)
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
    ) =
    Breakdown
        { velocity =
            zeroVelocity
                { ss = ss'
                , gs = Speed.startGateTaken startGates =<< ss'
                , es = es'
                , ssDistance = dS
                , ssElapsed = ssT
                , gsElapsed = gsT
                , ssVelocity = liftA2 mkVelocity dS ssT
                , gsVelocity = liftA2 mkVelocity dS gsT
                }
        , breakdown = x
        , total = TaskPoints $ r + dp + l + a + tp
        , place = TaskPlacing 0
        , reachDistance = dN
        , landedDistance = dL
        }
    where
        ss' = getTagTime unStart
        es' = getTagTime unEnd
        getTagTime accessor =
            (time :: Fix -> _)
            <$> (accessor =<< g)

mkVelocity
    :: PilotDistance (Quantity Double [u| km |])
    -> PilotTime (Quantity Double [u| h |])
    -> PilotVelocity (Quantity Double [u| km / h |])
mkVelocity (PilotDistance d) (PilotTime t) =
    PilotVelocity $ d /: t

startEnd :: RaceSections (Maybe Fix) -> StartEndTags
startEnd RaceSections{race} =
    case (race, reverse race) of
        ([], _) -> StartEnd Nothing Nothing
        (_, []) -> StartEnd Nothing Nothing
        (x : _, y : _) -> StartEnd x y

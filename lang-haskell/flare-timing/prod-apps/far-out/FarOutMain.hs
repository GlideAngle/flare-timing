{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Monad.Zip (munzip)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Route (OptimalRoute(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CompSettings(..)
    , Nominal(..)
    , MaskReachFile(..)
    , PilotGroup(didFlyNoTracklog)
    , IxTask(..)
    , compToTaskLength
    , compToMaskReach
    , compToFar
    , findCompInput
    , ensureExt
    )
import Flight.Distance (unTaskDistanceAsKm, fromKms)
import Flight.Track.Distance (TrackDistance(..), Effort)
import Flight.Track.Mask (MaskingEffort(..), MaskingReach(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import qualified Flight.Lookup as Lookup (compRoutes)
import Flight.Scribe (readComp, readRoute, readMaskingReach, writeFaring)
import "flight-gap-allot" Flight.Score
    (FlownMax(..), PilotDistance(..), MinimumDistance(..), Pilot)
import "flight-gap-effort" Flight.Score (Difficulty(..), mergeChunks)
import qualified "flight-gap-effort" Flight.Score as Gap
    (Chunking(..), ChunkDifficulty(..), landouts, lookahead, gradeDifficulty)
import "flight-gap-valid" Flight.Score (ReachStats(..))
import Flight.Lookup.Route (routeLength)
import FarOutOptions (description)
import MaskPilots (didFlyNoTrackStats)
import Stats (FlightStats(..))

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
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
    fprint ("Far outs counted for distance difficulty completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let maskReachFile@(MaskReachFile maskReachPath) = compToMaskReach compFile
    let farFile = compToFar compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading far outs from '" ++ takeFileName maskReachPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readMaskingReach maskReachFile)
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
                startRoute
                routes

    case (compSettings, masking, routes) of
        (Nothing, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the routes."
        (_, _, Nothing) -> putStrLn "Couldn't read the maskings."
        (Just cs, Just mk, Just _) -> do
            let CompSettings{nominal = Cmp.Nominal{free}, tasks, pilotGroups} = cs
            let ixTasks = take (length tasks) (IxTask <$> [1 ..])

            let dfNtss =
                    didFlyNoTrackStats
                        free
                        tasks
                        (Lookup.compRoutes lookupTaskLength ixTasks)
                        (didFlyNoTracklog <$> pilotGroups)

            let ess =
                    [
                        catMaybes
                        [ (p,) <$> statEffort
                        | (p, FlightStats{statEffort}) <- dfNts
                        ]

                    | dfNts <- dfNtss
                    ]

            writeFaring farFile $ difficultyByReach cs mk ess

difficultyByReach
    :: CompSettings k
    -> MaskingReach
    -> [[(Pilot, TrackDistance Effort)]]
    -> Cmp.Landing
difficultyByReach cs MaskingReach{bolster, nigh} dfNtss =
    difficulty
        cs
        MaskingEffort
            { bestEffort =
                [ do
                    ReachStats{max = FlownMax d} <- b
                    return $ fromKms d

                | b <- bolster
                ]
            , land = zipWith (++) xss dfNtss
            }
    where
        xss =
                [
                    [ (p,) . (\x -> TrackDistance Nothing x) $ made
                    | (p, TrackDistance{made}) <- ns
                    ]
                | ns <- nigh
                ]

difficulty :: CompSettings k -> MaskingEffort -> Cmp.Landing
difficulty CompSettings{nominal} MaskingEffort{bestEffort, land} =
    Cmp.Landing
        { minDistance = md
        , bestDistance = bests
        , landout = length <$> land
        , lookahead = ahead
        , chunking = cgs
        , difficulty = cs
        }
    where
        md@(MinimumDistance (MkQuantity free')) = free nominal

        pss :: [[Pilot]]
        pss = (fmap . fmap) fst land

        dss :: [[PilotDistance (Quantity Double [u| km |])]]
        dss =
            (fmap . fmap)
            (PilotDistance
            . MkQuantity
            . (\TrackDistance{made} ->
                -- NOTE: Anyone not having a made distance is given the free one.
                maybe free' unTaskDistanceAsKm made)
            . snd)
            land

        bests :: [Maybe (FlownMax (Quantity Double [u| km |]))]
        bests =
            (fmap . fmap) (FlownMax . MkQuantity . unTaskDistanceAsKm) bestEffort

        ahead =
            [ flip Gap.lookahead ds <$> b
            | b <- bests
            | ds <- dss
            ]

        (cgs, es) :: ([Maybe Gap.Chunking], [Maybe Difficulty]) =
            unzip $
            munzip <$>
            [ (\bd -> Gap.gradeDifficulty bd ps ds) <$> b
            | b <- bests
            | ps <- pss
            | ds <- dss
            ]

        cs :: [Maybe [Gap.ChunkDifficulty]] =
            [ do
                is' <- is
                js' <- js
                ks' <- ks
                dws' <- dws
                rs' <- rs
                mergeChunks los is' js' ks' dws' rs' <$> fs
            | los <- [Gap.landouts ps ds | ps <- pss| ds <- dss]
            | is <- (fmap . fmap) startChunk es
            | js <- (fmap . fmap) endChunk es
            | ks <- (fmap . fmap) endAhead es
            | dws <- fmap downward <$> es
            | rs <- fmap relative <$> es
            | fs <- fmap fractional <$> es
            ]

import Debug.Trace (traceShowId)
import Prelude hiding (span)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes, isNothing)
import Data.List (nub, sort)
import Control.Lens ((^?), element)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)
import Control.Concurrent.ParallelIO (parallel)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , CompTaskSettings(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail(..)
    , IxTask(..)
    , Task(..)
    , Comp(..)
    , findCompInput
    , reshape
    , pilotNamed
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Units ()
import Flight.Track.Cross
    ( TrackFlyingSection(..)
    , CompFlying(..)
    )
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Mask
    ( FnIxTask
    , FnTask
    , MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , SelectedStart(..)
    , NomineeStarts(..)
    , ExcludedCrossings(..)
    , settingsLogs
    , madeZones
    , nullFlying
    )
import Flight.TrackLog (pilotTrack)
import Flight.Scribe (readCompAndTasks, writeCompFlyTime)
import FlyTimeOptions (description)
import Flight.Span.Math (Math(..))

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
    fprint ("Tracks crossing zones completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{pilot, math, task} compFile = do
    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    case filesTaskAndSettings of
        Nothing -> putStrLn "Couldn't read the comp settings."
        Just (taskFiles, settings@(cs, _)) -> do
            let inFiles = (compFile, taskFiles)
            let CompTaskSettings{comp, tasks} = uncurry mkCompTaskSettings$ settings
            let ixSelectTasks = IxTask <$> task
            let ps = pilotNamed cs $ PilotName <$> pilot
            (_, selectedCompLogs) <- settingsLogs inFiles ixSelectTasks ps

            tracks :: [[Either (Pilot, TrackFileFail) (Pilot, MadeZones)]] <-
                    sequence $
                    [
                        parallel $
                        [ runExceptT $ pilotTrack ((flown comp math) tasks ixTask) (traceShowId pilotLog)
                        | pilotLog <- taskLogs
                        ]

                    | ixTask <- IxTask <$> [1..]
                    | taskLogs <- selectedCompLogs
                    ]

            writeFlyings compFile tasks tracks

writeFlyings
    :: CompInputFile
    -> [Task k]
    -> [[Either (Pilot, TrackFileFail) (Pilot, MadeZones)]]
    -> IO ()
writeFlyings compFile _ xs = do
    let ys :: [([(Pilot, Maybe MadeZones)], [Maybe (Pilot, TrackFileFail)])] =
            unzip <$>
            (fmap . fmap)
                (\case
                    Left err@(p, _) ->
                        ((p, Nothing), Just err)

                    Right (p, x) ->
                        ((p, Just x), Nothing))
                xs

    let pss = fst <$> ys
    let ess = catMaybes . snd <$> ys

    let pErrs :: [[Pilot]] =
            [ fst <$> filter ((/= TrackLogFileNotSet) . snd) es
            | es <- ess
            ]

    let flying = (fmap . fmap . fmap . fmap) madeZonesToFlying pss

    let notFlys :: [[Pilot]] =
            [ fmap fst . filter snd
              $ (fmap . fmap) (maybe False (not . flew)) fs
            | fs <- flying
            ]

    let dnfs =
            [ sort . nub $ es ++ ns
            | es <- pErrs
            | ns <- notFlys
            ]

    let flyTime =
            CompFlying
                { suspectDnf = dnfs
                , flying = flying
                }

    writeCompFlyTime compFile flyTime

flew :: TrackFlyingSection -> Bool
flew TrackFlyingSection{flyingFixes, flyingSeconds}
    | isNothing flyingFixes = False
    | isNothing flyingSeconds = False
    | otherwise = f flyingFixes || f flyingSeconds
    where
        f :: Ord a => Maybe (a, a) -> Bool
        f Nothing = False
        f (Just (a, b)) = a < b

madeZonesToFlying :: MadeZones -> TrackFlyingSection
madeZonesToFlying MadeZones{flying} = flying

flown :: Comp -> Math -> FnIxTask k MadeZones
flown c math tasks (IxTask i) fs =
    case tasks ^? element (i - 1) of
        Nothing ->
            MadeZones
                { flying = nullFlying
                , selectedCrossings = SelectedCrossings []
                , nomineeCrossings = NomineeCrossings []
                , selectedStart = SelectedStart Nothing
                , nomineeStarts = NomineeStarts []
                , excludedCrossings = ExcludedCrossings []
                }

        Just task ->
            flownTask c math task fs

flownTask :: Comp -> Math -> FnTask k MadeZones
flownTask Comp{earth, earthMath, give} math task =
    -- WARNING: FS doesn't exclude crossings if they are outside zone time
    -- windows or if they jump the gun by too much.
    f give (repeat $ const True) task
    where
        f = case ((earthMath, earth), math) of
                (e@(Pythagorus, EarthAsFlat{}), Floating) ->
                    madeZones @Double @Double e
                (e@(Haversines, EarthAsSphere{}), Floating) ->
                    madeZones @Double @Double e
                (e@(Vincenty, EarthAsEllipsoid{}), Floating) ->
                    madeZones @Double @Double e
                (e@(AndoyerLambert, EarthAsEllipsoid{}), Floating) ->
                    madeZones @Double @Double e
                (e@(ForsytheAndoyerLambert, EarthAsEllipsoid{}), Floating) ->
                    madeZones @Double @Double e
                (e@(FsAndoyer, EarthAsEllipsoid{}), Floating) ->
                    madeZones @Double @Double e
                -- TODO: Implement rational instances of GeoTag typeclass.
                -- No instance for (Flight.Mask.Tag.GeoTag Rational Rational)
                -- arising from a use of ‘madeZones’
                _ -> error "Combination of Earth model and math not yet implemented."

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)

import Flight.Route (OptimalRoute(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , PilotName(..)
    , IxTask(..)
    , CompTaskSettings(..)
    , Tweak(..)
    , findCompInput
    , reshape
    , pilotNamed
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Track.Stop (effectiveTagging)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Lookup.Stop (stopFlying)
import Flight.Lookup.Tag (tagTaskLeading)
import Flight.Scribe
    (readCompAndTasks, readRoutes, readCompTagZone, readCompPegFrame, readCompMaskArrival)
import Flight.Lookup.Route (routeLength)
import MaskReachOptions (description)
import Mask (writeMask)
import Flight.Track.Lead (sumAreas)
import "flight-gap-lead" Flight.Score (mk1Coef, mk2Coef, area1toCoef, area2toCoef)

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
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> readCompTagZone compFile)
            (const $ return Nothing)

    stopping <-
        catchIO
            (Just <$> readCompPegFrame compFile)
            (const $ return Nothing)

    arriving <-
        catchIO
            (Just <$> readCompMaskArrival compFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoutes compFile)
            (const $ return Nothing)

    let scoredLookup = stopFlying stopping
    let lookupTaskLength =
            routeLength
                taskRoute
                taskRouteSpeedSubset
                stopRoute
                startRoute
                routes

    case (filesTaskAndSettings, tagging, stopping, arriving, routes) of
        (Nothing, _, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing, _, _) -> putStrLn "Couldn't read the scored frames."
        (_, _, _, Nothing, _) -> putStrLn "Couldn't read the arrivals."
        (_, _, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just (taskFiles, settings@(cs, _)), Just tg, Just stp, Just as, Just _) -> do
            let cts@CompTaskSettings{compTweak} = uncurry mkCompTaskSettings settings
            let tagging' = Just $ effectiveTagging tg stp

            let lc1 = writeMask as sumAreas (mk1Coef . area1toCoef) area1toCoef
            let lc2 = writeMask as sumAreas (mk2Coef . area2toCoef) area2toCoef

            (if maybe True leadingAreaDistanceSquared compTweak then lc2 else lc1)
                cts
                lookupTaskLength
                math
                scoredLookup
                tagging'
                (tagTaskLeading tagging')
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                (compFile, taskFiles)

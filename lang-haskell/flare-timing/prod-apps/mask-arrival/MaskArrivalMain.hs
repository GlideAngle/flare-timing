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
    , compToPeg
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
import Flight.Scribe
    (readCompAndTasks, readRoutes, readCompTagZone, readCompPegFrame)
import Flight.Lookup.Route (routeLength)
import MaskArrivalOptions (description)
import Mask (writeMask)

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
    fprint ("Masking tracks for arrivals completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{math, task, pilot} compFile = do
    let stopFile = compToPeg compFile
    putStrLn $ "Reading scored times from " ++ show stopFile

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
            (Just <$> readCompPegFrame stopFile)
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

    case (filesTaskAndSettings, tagging, stopping, routes) of
        (Nothing, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing, _) -> putStrLn "Couldn't read the scored frames."
        (_, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just (taskFiles, settings@(cs, _)), Just tg, Just stp, Just _) -> do
            let iTasks = (IxTask <$> task)
            let ps = (pilotNamed cs $ PilotName <$> pilot)
            let tagging' = Just $ effectiveTagging tg stp

            writeMask
                (uncurry mkCompTaskSettings $ settings)
                lookupTaskLength
                math
                scoredLookup
                tagging'
                iTasks
                ps
                (compFile, taskFiles)

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Units.Angle (Angle(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(tasks)
    , Task(zones, speedSection)
    , CompInputFile(..)
    , compToTaskLength
    , findCompInput
    , ensureExt
    )
import Flight.TaskTrack.Double (taskTracks)
import Flight.Scribe (readComp, writeRoute)
import Flight.Zone.MkZones (unkindZones)
import Flight.Zone (Zone(..), Bearing(..), center)
import Flight.Earth.Sphere.PointToPoint.Double (azimuthFwd)
import TaskLengthOptions (CmdOptions(..), mkOptions)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Measuring task lengths completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ takeFileName compPath

    settings <- readComp compFile
    f settings
    where
        f compInput = do
            let ixs = speedSection <$> tasks compInput
            let zss = unlineZones . unkindZones . zones <$> tasks compInput
            let includeTask = if null task then const True else flip elem task

            writeRoute
                (compToTaskLength compFile)
                (taskTracks noTaskWaypoints includeTask measure ixs zss)

-- | Make sure all Line and SemiCircle zones have their normals fixed.
unlineZones :: [Zone Double] -> [Zone Double]
unlineZones zs =
    -- WARNING: Assume for now that lines and semicircles are only at goal.
    -- TODO: Handle lines and semicircles at the end of the speed section.
    case reverse zs of
        (Line Nothing r o) : xM : xs ->
            let az = Bearing . normalize <$> azimuthFwd o (center xM) in
            reverse $ (Line az r o) : xM : xs

        (SemiCircle Nothing r o) : xM : xs ->
            let az = Bearing . normalize <$> azimuthFwd o (center xM) in
            reverse $ (SemiCircle az r o) : xM : xs

        _ -> zs

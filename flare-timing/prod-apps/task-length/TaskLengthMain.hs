import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(tasks)
    , Task(zones, speedSection)
    , Zones(..)
    , CompInputFile(..)
    , compToTaskLength
    , findCompInput
    , ensureExt
    )
import Flight.TaskTrack.Double (taskTracks)
import Flight.Scribe (readComp, writeRoute)
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
            let zss = raw . zones <$> tasks compInput
            let includeTask = if null task then const True else flip elem task

            writeRoute
                (compToTaskLength compFile)
                (taskTracks noTaskWaypoints includeTask measure ixs zss)

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , PilotName(..)
    , IxTask(..)
    , CompSettings(..)
    , Comp(..)
    , compToCross
    , crossToTag
    , tagToPeg
    , findCompInput
    , reshape
    , pilotNamed
    )
import Flight.Scribe (readCompAndTasks, compFileToTaskFiles, readTagging, readFraming)
import Flight.Lookup.Stop (stopFlying)
import AlignTimeOptions (description)
import Flight.Time.Align (checkAll, writeTime)

sp :: SampleParams Double
sp =
    SampleParams
        (Samples <$> replicate 6 3)
        (Tolerance 0.03)

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
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    Fmt.fprint ("Aligning times completed in " Fmt.% timeSpecs Fmt.% "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    let tagFile = crossToTag $ compToCross compFile
    let stopFile = tagToPeg tagFile
    putStrLn $ "Reading competition from " ++ show compFile
    putStrLn $ "Reading zone tags from " ++ show tagFile
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
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    stopping <-
        catchIO
            (Just <$> readFraming stopFile)
            (const $ return Nothing)

    let scoredLookup = stopFlying stopping

    case (filesTaskAndSettings, tagging, stopping) of
        (Nothing, _, _) -> putStrLn "Couldn't find the task files or read the settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing) -> putStrLn "Couldn't read the scored frame."
        (Just (taskFiles, (cs@CompSettings{comp = Comp{earthMath, give}}, _)), Just t, Just _) ->
            let f =
                    writeTime
                        (IxTask <$> task)
                        (pilotNamed cs $ PilotName <$> pilot)
                        (compFile, taskFiles)

            in (f . checkAll math earthMath give sp speedSectionOnly scoredLookup) t

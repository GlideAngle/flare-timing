{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , TagZoneFile(..)
    , StopCrossFile(..)
    , PilotName(..)
    , IxTask(..)
    , compToCross
    , crossToTag
    , tagToStop
    , findCompInput
    , ensureExt
    , pilotNamed
    )
import Flight.Scribe (readComp, readTagging, readFraming)
import Flight.Lookup.Stop (stopFlying)
import AlignTimeOptions (description)
import Flight.Time.Align (checkAll, writeTime)

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
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Aligning times completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let stopFile@(StopCrossFile stopPath) = tagToStop tagFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading scored times from '" ++ takeFileName stopPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
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

    case (compSettings, tagging, stopping) of
        (Nothing, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing) -> putStrLn "Couldn't read the scored frame."
        (Just cs, Just t, Just _) ->
            let f =
                    writeTime
                        (IxTask <$> task)
                        (pilotNamed cs $ PilotName <$> pilot)
                        (CompInputFile compPath)
            in (f . checkAll speedSectionOnly scoredLookup) t

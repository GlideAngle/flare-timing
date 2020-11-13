{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)

import Flight.Route (OptimalRoute(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , MaskArrivalFile(..)
    , PilotName(..)
    , IxTask(..)
    , CompSettings(..)
    , Tweak(..)
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , crossToTag
    , tagToPeg
    , findCompInput
    , ensureExt
    , pilotNamed
    )
import Flight.Track.Stop (effectiveTagging)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Lookup.Stop (stopFlying)
import Flight.Lookup.Tag (tagTaskLeading)
import Flight.Scribe (readComp, readRoute, readTagging, readFraming, readMaskingArrival)
import Flight.Lookup.Route (routeLength)
import MaskEffortOptions (description)
import Mask.Mask (writeMask, check)
import Flight.Track.Lead (sumAreas)
import "flight-gap-lead" Flight.Score (mk1Coef, mk2Coef, area1toCoef, area2toCoef)

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
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let stopFile@(PegFrameFile stopPath) = tagToPeg tagFile
    let maskArrivalFile@(MaskArrivalFile maskArrivalPath) = compToMaskArrival compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading scored times from '" ++ takeFileName stopPath ++ "'"
    putStrLn $ "Reading arrivals from '" ++ takeFileName maskArrivalPath ++ "'"

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

    arriving <-
        catchIO
            (Just <$> readMaskingArrival maskArrivalFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    let scoredLookup = stopFlying stopping
    let lookupTaskLength =
            routeLength
                taskRoute
                taskRouteSpeedSubset
                stopRoute
                startRoute
                routes

    case (compSettings, tagging, stopping, arriving, routes) of
        (Nothing, _, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing, _, _) -> putStrLn "Couldn't read the scored frames."
        (_, _, _, Nothing, _) -> putStrLn "Couldn't read the arrivals."
        (_, _, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just cs, Just tg, Just stp, Just as, Just _) -> do
            let iTasks = (IxTask <$> task)
            let ps = (pilotNamed cs $ PilotName <$> pilot)
            let tagging' = Just $ effectiveTagging tg stp
            let ttl = tagTaskLeading tagging'

            let lc1 chk = do
                    let invert = mk1Coef . area1toCoef

                    _ <- writeMask as sumAreas invert area1toCoef math cs lookupTaskLength ttl iTasks ps compFile chk
                    return ()

            let lc2 chk = do
                    let invert = mk2Coef . area2toCoef

                    _ <- writeMask as sumAreas invert area2toCoef math cs lookupTaskLength ttl iTasks ps compFile chk
                    return ()

            let CompSettings{compTweak} = cs
            let lc = if maybe True leadingAreaDistanceSquared compTweak then lc2 else lc1
            lc (check math lookupTaskLength scoredLookup tagging')

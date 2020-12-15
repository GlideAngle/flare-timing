import Debug.Trace
import Prelude hiding (abs)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)

import Flight.Units ()
import Flight.Scribe
    ( readComp, readAltArrival, readAltLandout, readAltRoute, readAltScore
    , readRoute, readCrossing, readTagging, readFraming
    , readMaskingArrival
    , readMaskingEffort
    , readDiscardingLead
    , readMaskingLead
    , readMaskingReach
    , readMaskingSpeed
    , readBonusReach
    , readLanding, readFaring, readPointing
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( AltDot(AltFs, AltAs)
    , FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , LeadAreaFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , FarOutFile(..)
    , GapPointFile(..)
    , AltArrivalFile(..)
    , AltLandoutFile(..)
    , AltRouteFile(..)
    , AltScoreFile(..)
    , findCompInput
    , compToAltArrival
    , compToAltLandout
    , compToAltRoute
    , compToAltScore
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , compToMaskEffort
    , compToLeadArea
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    , compToLand
    , compToFar
    , compToPoint
    , crossToTag
    , tagToPeg
    , reshape
    )
import qualified ServeOptions as Opt (description)
import Serve.Config
import Serve.App

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) Opt.description Nothing

    let lf = LenientFile {coerceFile = reshape CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdServeOptions -> IO ()
drive o@CmdServeOptions{file} = do
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files

go :: CmdServeOptions -> CompInputFile -> IO ()
go CmdServeOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag crossFile
    let stopFile@(PegFrameFile stopPath) = tagToPeg tagFile
    let maskArrivalFile@(MaskArrivalFile maskArrivalPath) = compToMaskArrival compFile
    let maskEffortFile@(MaskEffortFile maskEffortPath) = compToMaskEffort compFile
    let leadAreaFile@(LeadAreaFile leadAreaPath) = compToLeadArea compFile
    let maskLeadFile@(MaskLeadFile maskLeadPath) = compToMaskLead compFile
    let maskReachFile@(MaskReachFile maskReachPath) = compToMaskReach compFile
    let maskSpeedFile@(MaskSpeedFile maskSpeedPath) = compToMaskSpeed compFile
    let bonusReachFile@(BonusReachFile bonusReachPath) = compToBonusReach compFile
    let landFile@(LandOutFile _landPath) = compToLand compFile
    let farFile@(FarOutFile landPath) = compToFar compFile
    let pointFile@(GapPointFile pointPath) = compToPoint compFile
    let altFsArrivalFile@(AltArrivalFile altFsArrivalPath) = compToAltArrival AltFs compFile
    let altFsLandoutFile@(AltLandoutFile altFsLandoutPath) = compToAltLandout AltFs compFile
    let altFsRouteFile@(AltRouteFile altFsRoutePath) = compToAltRoute AltFs compFile
    let altFsScoreFile@(AltScoreFile altFsScorePath) = compToAltScore AltFs compFile
    let altAsScoreFile@(AltScoreFile altAsScorePath) = compToAltScore AltAs compFile
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading competition & pilots DNF from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading scored section from '" ++ takeFileName stopPath ++ "'"
    putStrLn $ "Reading arrivals from '" ++ takeFileName maskArrivalPath ++ "'"
    putStrLn $ "Reading effort from '" ++ takeFileName maskEffortPath ++ "'"
    putStrLn $ "Reading leading area from '" ++ takeFileName leadAreaPath ++ "'"
    putStrLn $ "Reading leading from '" ++ takeFileName maskLeadPath ++ "'"
    putStrLn $ "Reading reach from '" ++ takeFileName maskReachPath ++ "'"
    putStrLn $ "Reading speed from '" ++ takeFileName maskSpeedPath ++ "'"
    putStrLn $ "Reading bonus reach from '" ++ takeFileName bonusReachPath ++ "'"
    putStrLn $ "Reading land outs from '" ++ takeFileName landPath ++ "'"
    putStrLn $ "Reading scores from '" ++ takeFileName pointPath ++ "'"
    putStrLn $ "Reading FS arrivals from '" ++ takeFileName altFsArrivalPath ++ "'"
    putStrLn $ "Reading FS land outs from '" ++ takeFileName altFsLandoutPath ++ "'"
    putStrLn $ "Reading FS optimal routes from '" ++ takeFileName altFsRoutePath ++ "'"
    putStrLn $ "Reading FS scores from '" ++ takeFileName altFsScorePath ++ "'"
    putStrLn $ "Reading airScore scores from '" ++ takeFileName altAsScorePath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    case compSettings of
        Nothing -> putStrLn "Couldn't read the comp settings"
        Just cs -> do
            let cfg = nullConfig compFile cs

            routes <-
                catchIO
                    (Just <$> readRoute lenFile)
                    (const $ return Nothing)

            crossing <-
                catchIO
                    (Just <$> readCrossing crossFile)
                    (const $ return Nothing)

            tagging <-
                catchIO
                    (Just <$> readTagging tagFile)
                    (const $ return Nothing)

            framing <-
                catchIO
                    (Just <$> readFraming stopFile)
                    (const $ return Nothing)

            maskingArrival <-
                catchIO
                    (Just <$> readMaskingArrival maskArrivalFile)
                    (const $ return Nothing)

            maskingEffort <-
                catchIO
                    (Just <$> readMaskingEffort maskEffortFile)
                    (const $ return Nothing)

            discardingLead2 <-
                catchIO
                    (Just <$> readDiscardingLead leadAreaFile)
                    (const $ return Nothing)

            maskingLead <-
                catchIO
                    (Just <$> readMaskingLead maskLeadFile)
                    (const $ return Nothing)

            maskingReach <-
                catchIO
                    (Just <$> readMaskingReach maskReachFile)
                    (const $ return Nothing)

            bonusReach <-
                catchIO
                    (Just <$> readBonusReach bonusReachFile)
                    (const $ return Nothing)

            maskingSpeed <-
                catchIO
                    (Just <$> readMaskingSpeed maskSpeedFile)
                    (const $ return Nothing)

            _landing <-
                catchIO
                    (Just <$> readLanding landFile)
                    (const $ return Nothing)

            landing <-
                catchIO
                    (Just <$> readFaring farFile)
                    (const $ return Nothing)

            pointing <-
                catchIO
                    (Just <$> readPointing pointFile)
                    (const $ return Nothing)

            altFsA <-
                catchIO
                    (Just <$> readAltArrival altFsArrivalFile)
                    (const $ return Nothing)

            altFsL <-
                catchIO
                    (Just <$> readAltLandout altFsLandoutFile)
                    (const $ return Nothing)

            altFsR <-
                catchIO
                    (Just <$> readAltRoute altFsRouteFile)
                    (const $ return Nothing)

            altFsS <-
                catchIO
                    (Just <$> readAltScore (traceShowId altFsScoreFile))
                    (const $ return Nothing)

            -- WARNING: Reading airScore's scores fails with
            -- AesonException "Error in $.score[0][0][1].landedMade: expected String, encountered Null"
            altAsS <-
                catchIO
                    (Just <$> readAltScore (traceShowId altAsScoreFile))
                    (const $ return Nothing)

            case (routes, crossing, tagging, framing, maskingArrival, maskingEffort, discardingLead2, maskingLead, maskingReach, maskingSpeed, bonusReach, landing, pointing) of
                (rt@(Just _), cg@(Just _), tg@(Just _), fm@(Just _), mA@(Just _), mE@(Just _), dL@(Just _), mL@(Just _), mR@(Just _), mS@(Just _), bR@(Just _), lo@(Just _), gp@(Just _)) ->
                    f =<< mkGapPointApp (Config compFile cs rt cg tg fm mA mE dL mL mR mS bR lo gp altFsA altFsL altFsR altFsS altAsS)
                (rt@(Just _), _, _, _, _, _, _, _, _, _, _, _, _) -> do
                    putStrLn "WARNING: Only serving comp inputs and task lengths"
                    f =<< mkTaskLengthApp cfg{routing = rt}
                (_, _, _, _, _, _, _, _, _, _, _, _, _) -> do
                    putStrLn "WARNING: Only serving comp inputs"
                    f =<< mkCompInputApp cfg
            where
                -- NOTE: Add gzip with wai gzip middleware.
                -- SEE: https://github.com/haskell-servant/servant/issues/786
                f = runSettings settings . gzip def

                port = 3000

                settings =
                    setPort port $
                    setBeforeMainLoop
                        (hPutStrLn stderr ("listening on port " ++ show port))
                        defaultSettings

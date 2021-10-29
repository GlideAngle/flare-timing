import Prelude hiding (abs)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)

import Flight.Units ()
import Flight.Scribe
    ( readCompAndTasks
    , readAltArrival, readAltLandOut, readAltRoute, readAltScore, readAsScore
    , readRoutes
    , readCompFlyTime, readCompCrossZone, readCompTagZone, readCompPegFrame
    , readCompMaskArrival
    , readCompMaskEffort
    , readCompLeadArea
    , readCompMaskLead
    , readCompMaskReach
    , readCompMaskSpeed
    , readCompMaskBonus
    , readCompLandOut, readCompFarOut, readCompGapPoint
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( AltDot(AltFs, AltAs)
    , FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , findCompInput
    , compToAltArrival
    , compToAltLandout
    , compToAltRoute
    , compToAltScore
    , reshape
    , mkCompTaskSettings
    , compFileToTaskFiles
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
go CmdServeOptions{..} compFile = do
    let altFsArrivalFile = compToAltArrival AltFs compFile
    let altFsLandoutFile = compToAltLandout AltFs compFile
    let altFsRouteFile = compToAltRoute AltFs compFile
    let altFsScoreFile = compToAltScore AltFs compFile
    let altAsScoreFile = compToAltScore AltAs compFile
    putStrLn $ "Reading competition & pilots DNF from " ++ show compFile
    putStrLn $ "Reading FS arrivals from " ++ show altFsArrivalFile
    putStrLn $ "Reading FS land outs from " ++ show altFsLandoutFile
    putStrLn $ "Reading FS optimal routes from " ++ show altFsRouteFile
    putStrLn $ "Reading FS scores from " ++ show altFsScoreFile
    putStrLn $ "Reading airScore scores from " ++ show altAsScoreFile

    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    case filesTaskAndSettings of
        Nothing -> putStrLn "Couldn't find the task files or read the settings"
        Just (taskFiles', (comp, tasks)) -> do
            let cs = mkCompTaskSettings comp tasks
            let inFiles = (compFile, taskFiles')
            let cfg = nullConfig inFiles cs

            routes <-
                catchIO
                    (Just <$> readRoutes compFile)
                    (const $ return Nothing)

            flying <-
                catchIO
                    (Just <$> readCompFlyTime compFile)
                    (const $ return Nothing)

            crossing <-
                catchIO
                    (Just <$> readCompCrossZone compFile)
                    (const $ return Nothing)

            tagging <-
                catchIO
                    (Just <$> readCompTagZone compFile)
                    (const $ return Nothing)

            framing <-
                catchIO
                    (Just <$> readCompPegFrame compFile)
                    (const $ return Nothing)

            maskingArrival <-
                catchIO
                    (Just <$> readCompMaskArrival compFile)
                    (const $ return Nothing)

            maskingEffort <-
                catchIO
                    (Just <$> readCompMaskEffort compFile)
                    (const $ return Nothing)

            discardingLead2 <-
                catchIO
                    (Just <$> readCompLeadArea compFile)
                    (const $ return Nothing)

            maskingLead <-
                catchIO
                    (Just <$> readCompMaskLead compFile)
                    (const $ return Nothing)

            maskingReach <-
                catchIO
                    (Just <$> readCompMaskReach compFile)
                    (const $ return Nothing)

            bonusReach <-
                catchIO
                    (Just <$> readCompMaskBonus compFile)
                    (const $ return Nothing)

            maskingSpeed <-
                catchIO
                    (Just <$> readCompMaskSpeed compFile)
                    (const $ return Nothing)

            _landing <-
                catchIO
                    (Just <$> readCompLandOut compFile)
                    (const $ return Nothing)

            landing <-
                catchIO
                    (Just <$> readCompFarOut compFile)
                    (const $ return Nothing)

            pointing <-
                catchIO
                    (Just <$> readCompGapPoint compFile)
                    (const $ return Nothing)

            altFsA <-
                catchIO
                    (Just <$> readAltArrival altFsArrivalFile)
                    (const $ return Nothing)

            altFsL <-
                catchIO
                    (Just <$> readAltLandOut altFsLandoutFile)
                    (const $ return Nothing)

            altFsR <-
                catchIO
                    (Just <$> readAltRoute altFsRouteFile)
                    (const $ return Nothing)

            altFsS <-
                catchIO
                    (Just <$> readAltScore altFsScoreFile)
                    (const $ return Nothing)

            -- WARNING: Reading airScore's scores fails with
            -- AesonException "Error in $.score[0][0][1].landedMade: expected String, encountered Null"
            altAsS <-
                catchIO
                    (Just <$> readAsScore altAsScoreFile)
                    (const $ return Nothing)

            case (routes, flying, crossing, tagging, framing, maskingArrival, maskingEffort, discardingLead2, maskingLead, maskingReach, maskingSpeed, bonusReach, landing, pointing) of
                (rt@(Just _), fy@(Just _), cg@(Just _), tg@(Just _), fm@(Just _), mA@(Just _), mE@(Just _), dL@(Just _), mL@(Just _), mR@(Just _), mS@(Just _), bR@(Just _), lo@(Just _), gp@(Just _)) ->
                    f =<< mkGapPointApp (Config inFiles cs rt fy cg tg fm mA mE dL mL mR mS bR lo gp altFsA altFsL altFsR altFsS altAsS)
                (rt@(Just _), _, _, _, _, _, _, _, _, _, _, _, _, _) -> do
                    putStrLn "WARNING: Only serving comp inputs and task lengths"
                    f =<< mkTaskLengthApp cfg{routing = rt}
                (_, _, _, _, _, _, _, _, _, _, _, _, _, _) -> do
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

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Prelude hiding (last)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (nonEmpty, last)
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
import Flight.Zone.MkZones (Zones(..))
import Flight.Zone.Raw (RawZone(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , CompSettings(..)
    , Comp(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , findCompInput
    , ensureExt
    , pilotNamed
    )
import Flight.Track.Time (TimeToTick, glideRatio, altBonusTimeToTick, copyTimeToTick)
import Flight.Mask (checkTracks)
import Flight.Scribe
    ( readComp
    , readPilotAlignTimeWriteDiscardFurther
    , readPilotAlignTimeWritePegThenDiscard
    )
import DiscardFurtherOptions (description)

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
    fprint ("Filtering times completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    case compSettings of
        Nothing -> putStrLn "Couldn't read the comp settings."
        Just cs ->
            filterTime
                cs
                compFile
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                checkAll

filterTime
    :: CompSettings k
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> IO [[Either (Pilot, _) (Pilot, _)]])
    -> IO ()
filterTime
    CompSettings{comp = Comp{discipline = hgOrPg}, tasks}
    compFile selectTasks selectPilots f = do

    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just xs -> do
            let taskPilots :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            sequence_
                [
                    mapM_
                        (readPilotAlignTimeWriteDiscardFurther
                            copyTimeToTick
                            id
                            compFile
                            (includeTask selectTasks)
                            n)
                        pilots
                | n <- (IxTask <$> [1 .. ])
                | pilots <- taskPilots
                ]

            let altBonusesOnTime :: [TimeToTick] =
                    [
                        fromMaybe copyTimeToTick $ do
                            _ <- stopped
                            zs' <- nonEmpty zs
                            let RawZone{alt} = last zs'
                            altBonusTimeToTick (glideRatio hgOrPg) <$> alt

                    | Task{stopped, zones = Zones{raw = zs}} <- tasks
                    ]

            sequence_
                [
                    sequence
                    [ do
                        a <- readPilotAlignTimeWritePegThenDiscard
                                timeToTick
                                id
                                compFile
                                (includeTask selectTasks)
                                n
                                p
                        return $ (p, a)

                    | p <- pilots
                    ]
                | n <- (IxTask <$> [1 .. ])
                | pilots <- taskPilots
                | timeToTick <- altBonusesOnTime
                ]

checkAll
    :: CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO [[Either (Pilot, TrackFileFail) (Pilot, ())]]
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

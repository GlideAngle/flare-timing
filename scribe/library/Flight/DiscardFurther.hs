module Flight.DiscardFurther
    ( readDiscardFurther
    , writeDiscardFurther
    , readCompBestDistances
    , readCompLeading
    ) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Csv
    (Header, decodeByName, EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList, null, last)

import Flight.Track.Time
    ( TickRow(..), LeadClose(..), LeadAllDown(..), LeadArrival(..), TimeToTick
    , discard
    )
import Flight.Track.Mask (RaceTime(..))
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , CompInputFile(..)
    , AlignTimeDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , DiscardFurtherDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , discardFurtherDir
    , alignTimePath
    , compFileToCompDir
    )
import Flight.AlignTime (readAlignTime)
import Flight.Score (Leg)

readDiscardFurther
    :: (MonadThrow m, MonadIO m)
    => DiscardFurtherFile
    -> m (Header, Vector TickRow)
readDiscardFurther (DiscardFurtherFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

writeDiscardFurther :: DiscardFurtherFile -> [String] -> Vector TickRow -> IO ()
writeDiscardFurther (DiscardFurtherFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs $ V.toList xs

lastRow :: Vector TickRow -> Maybe TickRow
lastRow xs =
    if V.null xs then Nothing else Just $ V.last xs

readCompBestDistances
    :: CompInputFile
    -> (IxTask -> Bool)
    -> [[Pilot]]
    -> IO [[Maybe (Pilot, TickRow)]]
readCompBestDistances compFile includeTask =
    zipWithM
        (\ i ps ->
            if not (includeTask i)
               then return []
               else readTaskBestDistances compFile i ps)
        (IxTask <$> [1 .. ])

readTaskBestDistances
    :: CompInputFile
    -> IxTask
    -> [Pilot]
    -> IO [Maybe (Pilot, TickRow)]
readTaskBestDistances compFile i =
    mapM (readPilotBestDistance compFile i)

readPilotBestDistance
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> IO (Maybe (Pilot, TickRow))
readPilotBestDistance compFile (IxTask iTask) pilot = do
    (_, rows) <- readDiscardFurther (DiscardFurtherFile (dOut </> file))

    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (_, AlignTimeFile file) = alignTimePath dir iTask pilot
        (DiscardFurtherDir dOut) = discardFurtherDir dir iTask

readCompLeading
    :: [TimeToTick]
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> [IxTask]
    -> [Int -> Leg]
    -> [Maybe RaceTime]
    -> [[Pilot]]
    -> IO [[(Pilot, [TickRow])]]
readCompLeading timeToTicks lengths compFile select tasks toLegs raceTimes pilots =
    sequence
        [
            (readTaskLeading timeToTick lengths compFile select)
                task
                toLeg
                rt
                ps
        | timeToTick <- timeToTicks
        | task <- tasks
        | toLeg <- toLegs
        | rt <- raceTimes
        | ps <- pilots
        ]

readTaskLeading
    :: TimeToTick
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> [Pilot]
    -> IO [(Pilot, [TickRow])]
readTaskLeading timeToTick lengths compFile select iTask@(IxTask i) toLeg raceTime ps =
    if not (select iTask) then return [] else do
    _ <- createDirectoryIfMissing True dOut
    xs <- mapM (readPilotLeading timeToTick lengths compFile iTask toLeg raceTime) ps
    return $ zip ps xs
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir dOut) = discardFurtherDir dir i

readPilotLeading
    :: TimeToTick
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO [TickRow]
readPilotLeading _ _ _ _ _ Nothing _ = return []
readPilotLeading
    timeToTick
    (RoutesLookupTaskDistance lookupTaskLength)
    compFile iTask@(IxTask i) toLeg
    (Just raceTime)
    pilot = do

    (_, rows) <- readAlignTime (AlignTimeFile (dIn </> file))

    return $ (V.toList . discard timeToTick toLeg taskLength close down arrival) rows
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir i pilot
        taskLength = (fmap wholeTaskDistance . ($ iTask)) =<< lookupTaskLength

        close = do
            c <- leadClose raceTime
            return $ LeadClose c

        down = do
            d <- leadAllDown raceTime
            return $ LeadAllDown d

        arrival = do
            a <- leadArrival raceTime
            return $ LeadArrival a

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Discard
    ( readDiscardFurther
    , writeDiscardFurther
    , readCompBestDistances
    , readCompLeading
    ) where

import Data.List (zipWith4)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Control.Monad (join, zipWithM)
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
    (TickRow(..), LeadClose(..), LeadArrival(..), discard)
import Flight.Track.Mask (RaceTime(..))
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , CompInputFile(..)
    , AlignDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , DiscardDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , RouteLookup(..)
    , discardDir
    , alignPath
    , compFileToCompDir
    )
import Data.Aeson.Via.Scientific (ViaSci(..))
import Flight.Align (readAlignTime)
import Flight.Score (Leg)

readDiscardFurther :: DiscardFurtherFile -> ExceptT String IO (Header, Vector TickRow)
readDiscardFurther (DiscardFurtherFile csvPath) = do
    contents <- lift $ BL.readFile csvPath
    ExceptT . return $ decodeByName contents

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
    rows <-
        runExceptT
        $ readDiscardFurther (DiscardFurtherFile (dOut </> file))

    return $ (pilot,) <$> either (const Nothing) (lastRow . snd) rows
    where
        dir = compFileToCompDir compFile
        (_, AlignTimeFile file) = alignPath dir iTask pilot
        (DiscardDir dOut) = discardDir dir iTask

readCompLeading
    :: RouteLookup
    -> CompInputFile
    -> (IxTask -> Bool)
    -> [IxTask]
    -> [Int -> Leg]
    -> [Maybe RaceTime]
    -> [[Pilot]]
    -> IO [[(Pilot, [TickRow])]]
readCompLeading lengths compFile select tasks toLeg raceTimes pilots =
    sequence $ zipWith4
        (readTaskLeading lengths compFile select)
        tasks
        toLeg
        raceTimes
        pilots

readTaskLeading
    :: RouteLookup
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> [Pilot]
    -> IO [(Pilot, [TickRow])]
readTaskLeading lengths compFile select iTask@(IxTask i) toLeg raceTime ps =
    if not (select iTask) then return [] else do
    _ <- createDirectoryIfMissing True dOut
    xs <- mapM (readPilotLeading lengths compFile iTask toLeg raceTime) ps
    return $ zip ps xs
    where
        dir = compFileToCompDir compFile
        (DiscardDir dOut) = discardDir dir i

readPilotLeading
    :: RouteLookup
    -> CompInputFile
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO [TickRow]
readPilotLeading _ _ _ _ Nothing _ = return []
readPilotLeading
    (RouteLookup lookupTaskLength)
    compFile iTask@(IxTask i) toLeg
    (Just raceTime)
    pilot = do

    rows <- runExceptT $ readAlignTime (AlignTimeFile (dIn </> file))
    return $ either
        (const [])
        (V.toList . discard toLeg taskLength close arrival . snd)
        rows
    where
        dir = compFileToCompDir compFile
        (AlignDir dIn, AlignTimeFile file) = alignPath dir i pilot
        taskLength = join (($ iTask) <$> lookupTaskLength)

        close = do
            ViaSci c <- leadClose raceTime
            return $ LeadClose c

        arrival = do
            ViaSci a <- leadArrival raceTime
            return $ LeadArrival a

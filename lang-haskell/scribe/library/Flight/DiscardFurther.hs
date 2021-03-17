module Flight.DiscardFurther
    ( AltBonus(..)
    , readCompBestDistances
    , readPilotAlignTimeWriteDiscardFurther
    , readPilotAlignTimeWriteDiscardFurtherStop
    , readPilotDiscardFurther
    , readPilotDiscardFurtherStop
    , readDiscardFurther
    ) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Csv
    (Header, decodeByName, EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (toList, null, last, filter)

import Flight.Track.Time
    ( TimeRow(..), TickRow(..), TimeToTick, TickToTick
    , TickHeader(..)
    , timesToKeptTicks, tickHeader
    )
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , CompInputFile(..)
    , AlignTimeDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , DiscardFurtherStopFile(..)
    , DiscardFurtherDir(..)
    , DiscardFurtherStopDir(..)
    , discardFurtherDir
    , discardFurtherStopDir
    , alignTimePath
    , discardFurtherPath
    , discardFurtherStopPath
    , compFileToCompDir
    )
import Flight.AlignTime (readAlignTime)

data AltBonus = AltBonus Bool

readDiscardFurther
    :: (MonadThrow m, MonadIO m)
    => DiscardFurtherFile
    -> m (Header, Vector TickRow)
readDiscardFurther (DiscardFurtherFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

readDiscardFurtherStop
    :: (MonadThrow m, MonadIO m)
    => DiscardFurtherStopFile
    -> m (Header, Vector TickRow)
readDiscardFurtherStop (DiscardFurtherStopFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

writeDiscardFurther :: DiscardFurtherFile -> Vector TickRow -> IO ()
writeDiscardFurther (DiscardFurtherFile path) xs =
    L.writeFile path rows
    where
        (TickHeader hs) = tickHeader
        opts = defaultEncodeOptions {encUseCrLf = False}
        rows = encodeByNameWith opts hs $ V.toList xs

writeDiscardFurtherStop :: DiscardFurtherStopFile -> TickHeader -> Vector TickRow -> IO ()
writeDiscardFurtherStop (DiscardFurtherStopFile path) (TickHeader hs) xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        rows = encodeByNameWith opts hs $ V.toList xs

lastRow :: Vector TickRow -> Maybe TickRow
lastRow xs =
    if V.null xs then Nothing else Just $ V.last xs

readCompBestDistances
    :: AltBonus
    -> CompInputFile
    -> (IxTask -> Bool)
    -> [[Pilot]]
    -> IO [[Maybe (Pilot, TickRow)]]
readCompBestDistances altBonus compFile includeTask =
    zipWithM
        (\ ixTask ps ->
            if not (includeTask ixTask)
               then return []
               else readTaskBestDistances altBonus compFile ixTask ps)
        (IxTask <$> [1 .. ])

readTaskBestDistances
    :: AltBonus
    -> CompInputFile
    -> IxTask
    -> [Pilot]
    -> IO [Maybe (Pilot, TickRow)]
readTaskBestDistances altBonus compFile ixTask =
    mapM (readPilotBestDistance altBonus compFile ixTask)

readPilotBestDistance
    :: AltBonus
    -> CompInputFile
    -> IxTask
    -> Pilot
    -> IO (Maybe (Pilot, TickRow))
readPilotBestDistance (AltBonus False) compFile ixTask pilot = do
    (_, rows) <- readDiscardFurther $ (DiscardFurtherFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir ixTask pilot
readPilotBestDistance (AltBonus True) compFile ixTask pilot = do
    (_, rows) <- readDiscardFurtherStop (DiscardFurtherStopFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherStopDir path, DiscardFurtherStopFile file) = discardFurtherStopPath dir ixTask pilot

readAlignWriteDiscard
    :: (Vector TickRow -> IO a)
    -> AlignTimeFile
    -> FilePath
    -> TimeToTick
    -> TickToTick
    -> (TimeRow -> Bool)
    -> IO (Maybe (Vector TickRow))
readAlignWriteDiscard fWrite fileIn dOut timeToTick tickToTick selectRow = do
    _ <- createDirectoryIfMissing True dOut
    (_, timeRows :: Vector TimeRow) <- readAlignTime fileIn
    let keptTimeRows = V.filter selectRow timeRows
    let tickRows :: Vector TickRow = timesToKeptTicks timeToTick tickToTick keptTimeRows
    _ <- fWrite tickRows
    return $ Just tickRows

readPilotAlignTimeWriteDiscardFurther
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> TimeToTick
    -> TickToTick
    -> (TimeRow -> Bool)
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWriteDiscardFurther compFile ixTask pilot =
    readAlignWriteDiscard writer fileIn dOut
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir ixTask pilot
        fileIn = AlignTimeFile (dIn </> file)
        (DiscardFurtherDir dOut) = discardFurtherDir dir ixTask

        writer :: Vector TickRow -> IO ()
        writer xs = do
            let fileOut = DiscardFurtherFile (dOut </> file)
            putStrLn $ "\tReading " ++ show fileIn ++ ", writing " ++ show fileOut
            writeDiscardFurther fileOut xs

readPilotAlignTimeWriteDiscardFurtherStop
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> TimeToTick
    -> TickToTick
    -> (TimeRow -> Bool)
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWriteDiscardFurtherStop compFile ixTask pilot =
    readAlignWriteDiscard writer fileIn dOut
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir ixTask pilot
        fileIn = AlignTimeFile (dIn </> file)
        (DiscardFurtherStopDir dOut) = discardFurtherStopDir dir ixTask

        writer :: Vector TickRow -> IO ()
        writer xs = do
            let fileOut = DiscardFurtherStopFile (dOut </> file)
            putStrLn $ "\tReading " ++ show fileIn ++ ", writing " ++ show fileOut
            writeDiscardFurtherStop fileOut tickHeader xs

readPilotDiscardFurther :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotDiscardFurther compFile ixTask pilot = do
    let dir = compFileToCompDir compFile
    let (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir ixTask pilot
    (_, rows) <- readDiscardFurther (DiscardFurtherFile $ path </> file)
    return $ V.toList rows

readPilotDiscardFurtherStop :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotDiscardFurtherStop compFile ixTask pilot = do
    let dir = compFileToCompDir compFile
    let (DiscardFurtherStopDir path, DiscardFurtherStopFile file) = discardFurtherStopPath dir ixTask pilot
    (_, rows) <- readDiscardFurtherStop (DiscardFurtherStopFile $ path </> file)
    return $ V.toList rows

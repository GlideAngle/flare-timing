module Flight.DiscardFurther
    ( AltBonus(..)
    , readCompBestDistances
    , readPilotAlignTimeWriteDiscardFurther
    , readPilotAlignTimeWritePegThenDiscard
    , readPilotDiscardFurther
    , readPilotPegThenDiscard
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
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList, null, last)

import Flight.Track.Time
    ( TimeRow(..), TickRow(..), TimeToTick, TickToTick
    , discard2, allHeaders
    )
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , CompInputFile(..)
    , AlignTimeDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , PegThenDiscardFile(..)
    , DiscardFurtherDir(..)
    , PegThenDiscardDir(..)
    , discardFurtherDir
    , pegThenDiscardDir
    , alignTimePath
    , discardFurtherPath
    , pegThenDiscardPath
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

readPegThenDiscard
    :: (MonadThrow m, MonadIO m)
    => PegThenDiscardFile
    -> m (Header, Vector TickRow)
readPegThenDiscard (PegThenDiscardFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

writeDiscardFurther :: DiscardFurtherFile -> [String] -> Vector TickRow -> IO ()
writeDiscardFurther (DiscardFurtherFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs $ V.toList xs

writePegThenDiscard :: PegThenDiscardFile -> [String] -> Vector TickRow -> IO ()
writePegThenDiscard (PegThenDiscardFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
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
        (\ i ps ->
            if not (includeTask i)
               then return []
               else readTaskBestDistances altBonus compFile i ps)
        (IxTask <$> [1 .. ])

readTaskBestDistances
    :: AltBonus
    -> CompInputFile
    -> IxTask
    -> [Pilot]
    -> IO [Maybe (Pilot, TickRow)]
readTaskBestDistances altBonus compFile i =
    mapM (readPilotBestDistance altBonus compFile i)

readPilotBestDistance
    :: AltBonus
    -> CompInputFile
    -> IxTask
    -> Pilot
    -> IO (Maybe (Pilot, TickRow))
readPilotBestDistance (AltBonus False) compFile (IxTask iTask) pilot = do
    (_, rows) <- readDiscardFurther $ (DiscardFurtherFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir iTask pilot
readPilotBestDistance (AltBonus True) compFile (IxTask iTask) pilot = do
    (_, rows) <- readPegThenDiscard (PegThenDiscardFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (PegThenDiscardDir path, PegThenDiscardFile file) = pegThenDiscardPath dir iTask pilot

readPilotAlignTimeWriteDiscardFurther
    :: TimeToTick
    -> TickToTick
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> Pilot
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWriteDiscardFurther
    timeToTick
    tickToTick
    compFile
    selectTask
    iTask@(IxTask i) pilot =
    if not (selectTask iTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    (_, timeRows :: Vector TimeRow) <- readAlignTime (AlignTimeFile (dIn </> file))
    let tickRows :: Vector TickRow = discard2 timeToTick tickToTick timeRows
    _ <- f tickRows
    return $ Just tickRows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file) allHeaders
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir i pilot
        (DiscardFurtherDir dOut) = discardFurtherDir dir i

readPilotAlignTimeWritePegThenDiscard
    :: TimeToTick
    -> TickToTick
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> Pilot
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWritePegThenDiscard
    timeToTick
    tickToTick
    compFile
    selectTask
    iTask@(IxTask i) pilot =
    if not (selectTask iTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    (_, timeRows :: Vector TimeRow) <- readAlignTime (AlignTimeFile (dIn </> file))
    let tickRows :: Vector TickRow = discard2 timeToTick tickToTick timeRows
    _ <- f tickRows
    return $ Just tickRows
    where
        f = writePegThenDiscard (PegThenDiscardFile $ dOut </> file) allHeaders
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir i pilot
        (PegThenDiscardDir dOut) = pegThenDiscardDir dir i

readPilotDiscardFurther :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotDiscardFurther compFile (IxTask i) pilot = do
    let dir = compFileToCompDir compFile
    let (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir i pilot
    (_, rows) <- readDiscardFurther (DiscardFurtherFile $ path </> file)
    return $ V.toList rows

readPilotPegThenDiscard :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotPegThenDiscard compFile (IxTask i) pilot = do
    let dir = compFileToCompDir compFile
    let (PegThenDiscardDir path, PegThenDiscardFile file) = pegThenDiscardPath dir i pilot
    (_, rows) <- readPegThenDiscard (PegThenDiscardFile $ path </> file)
    return $ V.toList rows

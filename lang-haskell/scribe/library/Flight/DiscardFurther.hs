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

newtype AltBonus = AltBonus Bool

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

writeDiscardFurther :: DiscardFurtherFile -> Vector TickRow -> IO ()
writeDiscardFurther (DiscardFurtherFile path) xs =
    L.writeFile path rows
    where
        (TickHeader hs) = tickHeader
        opts = defaultEncodeOptions {encUseCrLf = False}
        rows = encodeByNameWith opts hs $ V.toList xs

writePegThenDiscard :: PegThenDiscardFile -> TickHeader -> Vector TickRow -> IO ()
writePegThenDiscard (PegThenDiscardFile path) (TickHeader hs) xs =
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
    (_, rows) <- readDiscardFurther (DiscardFurtherFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir ixTask pilot
readPilotBestDistance (AltBonus True) compFile ixTask pilot = do
    (_, rows) <- readPegThenDiscard (PegThenDiscardFile $ path </> file)
    return $ (pilot,) <$> lastRow rows
    where
        dir = compFileToCompDir compFile
        (PegThenDiscardDir path, PegThenDiscardFile file) = pegThenDiscardPath dir ixTask pilot

readPilotAlignTimeWriteDiscardFurther
    :: TimeToTick
    -> TickToTick
    -> CompInputFile
    -> (IxTask -> Bool)
    -> (TimeRow -> Bool)
    -> IxTask
    -> Pilot
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWriteDiscardFurther
    timeToTick
    tickToTick
    compFile
    selectTask
    selectRow
    ixTask pilot =
    if not (selectTask ixTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    (_, timeRows :: Vector TimeRow) <- readAlignTime (AlignTimeFile (dIn </> file))
    let keptTimeRows = V.filter selectRow timeRows
    let tickRows :: Vector TickRow = timesToKeptTicks timeToTick tickToTick keptTimeRows
    _ <- f tickRows
    return $ Just tickRows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file)
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir ixTask pilot
        (DiscardFurtherDir dOut) = discardFurtherDir dir ixTask

readPilotAlignTimeWritePegThenDiscard
    :: TimeToTick
    -> TickToTick
    -> CompInputFile
    -> (IxTask -> Bool)
    -> (TimeRow -> Bool)
    -> IxTask
    -> Pilot
    -> IO (Maybe (Vector TickRow))
readPilotAlignTimeWritePegThenDiscard
    timeToTick
    tickToTick
    compFile
    selectTask
    selectRow
    ixTask pilot =
    if not (selectTask ixTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    (_, timeRows :: Vector TimeRow) <- readAlignTime (AlignTimeFile (dIn </> file))
    let keptTimeRows = V.filter selectRow timeRows
    let tickRows :: Vector TickRow = timesToKeptTicks timeToTick tickToTick keptTimeRows
    _ <- f tickRows
    return $ Just tickRows
    where
        f = writePegThenDiscard (PegThenDiscardFile $ dOut </> file) tickHeader
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir ixTask pilot
        (PegThenDiscardDir dOut) = pegThenDiscardDir dir ixTask

readPilotDiscardFurther :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotDiscardFurther compFile ixTask pilot = do
    let dir = compFileToCompDir compFile
    let (DiscardFurtherDir path, DiscardFurtherFile file) = discardFurtherPath dir ixTask pilot
    (_, rows) <- readDiscardFurther (DiscardFurtherFile $ path </> file)
    return $ V.toList rows

readPilotPegThenDiscard :: CompInputFile -> IxTask -> Pilot -> IO [TickRow]
readPilotPegThenDiscard compFile ixTask pilot = do
    let dir = compFileToCompDir compFile
    let (PegThenDiscardDir path, PegThenDiscardFile file) = pegThenDiscardPath dir ixTask pilot
    (_, rows) <- readPegThenDiscard (PegThenDiscardFile $ path </> file)
    return $ V.toList rows

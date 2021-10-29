module Flight.UnpackTrack
    ( readUnpackTrack
    , writeUnpackTrack
    , readCompTrackRows
    ) where

import Control.DeepSeq
import Control.Exception.Safe (MonadThrow, throwString, catchIO)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    ( Header
    , EncodeOptions(..)
    , decodeByName
    , encodeByNameWith
    , defaultEncodeOptions
    )
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (toList)
import System.FilePath ((</>))

import Flight.Track.Time (TrackRow(..), TimeHeader(..), timeHeader)
import Flight.Comp
    ( CompInputFile(..)
    , UnpackTrackFile(..)
    , Pilot(..)
    , IxTask(..)
    , UnpackTrackDir(..)
    , UnpackTrackFile(..)
    , unpackTrackPath
    , compFileToCompDir
    )

readUnpackTrack
    :: (MonadThrow m, MonadIO m)
    => UnpackTrackFile
    -> m (Header, Vector TrackRow)
readUnpackTrack (UnpackTrackFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $!! decodeByName contents

writeUnpackTrack :: UnpackTrackFile -> [TrackRow] -> IO ()
writeUnpackTrack (UnpackTrackFile path) xs =
    L.writeFile path rows
    where
        (TimeHeader hs) = timeHeader
        opts = defaultEncodeOptions {encUseCrLf = False}
        rows = encodeByNameWith opts hs xs

readCompTrackRows
    :: CompInputFile
    -> (IxTask -> Bool)
    -> [[Pilot]]
    -> IO [[Maybe (Pilot, [TrackRow])]]
readCompTrackRows compFile includeTask =
    zipWithM
        (\ i ps ->
            if not (includeTask i)
               then return []
               else readTaskTrackRows compFile i ps)
        (IxTask <$> [1 .. ])

readTaskTrackRows
    :: CompInputFile
    -> IxTask
    -> [Pilot]
    -> IO [Maybe (Pilot, [TrackRow])]
readTaskTrackRows compFile ixTask =
    mapM
        (\p -> do
            rows <- catchIO (readPilotTrackRows compFile ixTask p) (const $ return [])
            return $ if null rows then Nothing else Just (p, rows))

readPilotTrackRows
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> IO [TrackRow]
readPilotTrackRows compFile ixTask pilot = do
    (_, rows) <- readUnpackTrack (UnpackTrackFile (dIn </> file))
    return $ V.toList rows
    where
        dir = compFileToCompDir compFile
        (UnpackTrackDir dIn, UnpackTrackFile file) = unpackTrackPath dir ixTask pilot

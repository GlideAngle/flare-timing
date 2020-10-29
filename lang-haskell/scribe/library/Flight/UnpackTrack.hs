module Flight.UnpackTrack
    ( readUnpackTrack
    , writeUnpackTrack
    , readCompTrackRows
    ) where

import Control.DeepSeq
import Control.Exception.Safe (MonadThrow, throwString, catchIO)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad (zipWithM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    ( Header
    , EncodeOptions(..)
    , decodeByName
    , encodeByNameWith
    , defaultEncodeOptions
    )
import qualified Data.ByteString.Char8 as S (writeFile, readFile)
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

-- SEE: https://gist.github.com/dino-/28b09c465c756c44b2c91d777408e166
mkLazy :: B.ByteString -> BL.ByteString
mkLazy = BL.fromChunks . return

readUnpackTrack
    :: (MonadThrow m, MonadIO m)
    => UnpackTrackFile
    -> m (Header, Vector TrackRow)
readUnpackTrack (UnpackTrackFile csvPath) = do
    contents <- liftIO $ S.readFile csvPath
    either throwString return $!! decodeByName (mkLazy contents)

writeUnpackTrack :: UnpackTrackFile -> [TrackRow] -> IO ()
writeUnpackTrack (UnpackTrackFile path) xs =
    S.writeFile path $ BL.toStrict rows
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
readTaskTrackRows compFile i =
    mapM
        (\p -> do
            rows <- catchIO (readPilotTrackRows compFile i p) (const $ return [])
            return $ if null rows then Nothing else Just (p, rows))

readPilotTrackRows
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> IO [TrackRow]
readPilotTrackRows compFile (IxTask i) pilot = do
    (_, rows) <- readUnpackTrack (UnpackTrackFile (dIn </> file))
    return $ V.toList rows
    where
        dir = compFileToCompDir compFile
        (UnpackTrackDir dIn, UnpackTrackFile file) = unpackTrackPath dir i pilot

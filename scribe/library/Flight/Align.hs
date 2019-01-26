module Flight.Align
    ( readAlignTime
    , writeAlignTime
    , readCompTimeRows
    ) where

import Control.Exception.Safe (MonadThrow, throwString)
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
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, find)
import System.FilePath ((</>))

import Flight.Track.Time (LeadTick(..), TimeRow(..))
import Flight.Comp
    ( CompInputFile(..)
    , AlignTimeFile(..)
    , Pilot(..)
    , IxTask(..)
    , AlignDir(..)
    , AlignTimeFile(..)
    , alignPath
    , compFileToCompDir
    )

readAlignTime
    :: (MonadThrow m, MonadIO m)
    => AlignTimeFile
    -> m (Header, Vector TimeRow)
readAlignTime (AlignTimeFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

writeAlignTime :: AlignTimeFile -> [String] -> [TimeRow] -> IO ()
writeAlignTime (AlignTimeFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs xs

readCompTimeRows
    :: CompInputFile
    -> (IxTask -> Bool)
    -> [[(Pilot, Maybe LeadTick)]]
    -> IO [[Maybe (Pilot, TimeRow)]]
readCompTimeRows compFile includeTask =
    zipWithM
        (\ i ps ->
            if not (includeTask i)
               then return []
               else readTaskTimeRows compFile i ps)
        (IxTask <$> [1 .. ])

readTaskTimeRows
    :: CompInputFile
    -> IxTask
    -> [(Pilot, Maybe LeadTick)]
    -> IO [Maybe (Pilot, TimeRow)]
readTaskTimeRows compFile i =
    mapM (uncurry $ readPilotTimeRow compFile i)

readPilotTimeRow
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> Maybe LeadTick
    -> IO (Maybe (Pilot, TimeRow))
readPilotTimeRow compFile (IxTask iTask) pilot mark = do
    (_, rows) <- readAlignTime (AlignTimeFile (dirIn </> file))

    return $
        ((fmap . fmap)
            (pilot,)
            (V.find (\TimeRow{tickLead} -> mark == tickLead)))
        rows
    where
        dir = compFileToCompDir compFile
        (AlignDir dirIn, AlignTimeFile file) = alignPath dir iTask pilot
module Flight.AlignTime
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
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (find)
import System.FilePath ((</>))

import Flight.Track.Time (LeadTick(..), TimeRow(..), TimeHeader(..), timeHeader)
import Flight.Comp
    ( CompInputFile(..)
    , AlignTimeFile(..)
    , Pilot(..)
    , IxTask(..)
    , AlignTimeDir(..)
    , AlignTimeFile(..)
    , alignTimePath
    , compFileToCompDir
    )

readAlignTime
    :: (MonadThrow m, MonadIO m)
    => AlignTimeFile
    -> m (Header, Vector TimeRow)
readAlignTime (AlignTimeFile csvPath) = do
    contents <- liftIO $ BL.readFile csvPath
    either throwString return $ decodeByName contents

writeAlignTime :: AlignTimeFile -> [TimeRow] -> IO ()
writeAlignTime (AlignTimeFile path) xs =
    L.writeFile path rows
    where
        TimeHeader hs = timeHeader
        opts = defaultEncodeOptions {encUseCrLf = False}
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
readPilotTimeRow compFile iTask pilot mark = do
    (_, rows) <- readAlignTime (AlignTimeFile (dirIn </> file))

    return $
        ((fmap . fmap)
            (pilot,)
            (V.find (\TimeRow{tickLead} -> mark == tickLead)))
        rows
    where
        dir = compFileToCompDir compFile
        (AlignTimeDir dirIn, AlignTimeFile file) = alignTimePath dir iTask pilot

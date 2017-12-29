{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Align
    ( readAlignTime
    , writeAlignTime
    , readCompTimeRows
    ) where

import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    (Header, decodeByName, EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
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

readAlignTime :: AlignTimeFile -> ExceptT String IO (Header, Vector TimeRow)
readAlignTime (AlignTimeFile csvPath) = do
    contents <- lift $ BL.readFile csvPath
    ExceptT . return $ decodeByName contents

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
readCompTimeRows compFile includeTask pss =
    zipWithM
        (\ i ps ->
            if not (includeTask i)
               then return []
               else readTaskTimeRows compFile i ps)
        (IxTask <$> [1 .. ])
        pss

readTaskTimeRows
    :: CompInputFile
    -> IxTask
    -> [(Pilot, Maybe LeadTick)]
    -> IO [Maybe (Pilot, TimeRow)]
readTaskTimeRows compFile i ps =
    mapM (uncurry $ readPilotTimeRow compFile i) ps

readPilotTimeRow
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> Maybe LeadTick
    -> IO (Maybe (Pilot, TimeRow))
readPilotTimeRow compFile (IxTask iTask) pilot mark = do
    rows <-
        runExceptT
        $ readAlignTime (AlignTimeFile (dirIn </> file))

    return $ either
        (const Nothing) 
        ((fmap . fmap)
            (pilot,)
            (V.find (\TimeRow{tickLead} -> mark == tickLead) . snd))
        rows
    where
        dir = compFileToCompDir compFile
        (AlignDir dirIn, AlignTimeFile file) = alignPath dir iTask pilot

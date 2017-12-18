module Flight.Yaml
    ( readComp, writeComp
    , readRoute, writeRoute
    , readCrossing , writeCrossing
    , readTagging, writeTagging
    , readAlignTime, writeAlignTime
    , readDiscardFurther, writeDiscardFurther
    , writeMasking
    ) where

import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import qualified Data.Yaml.Pretty as Y
import Data.Csv
    (Header, decodeByName, EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import qualified Data.Vector as V (fromList, toList)
import Flight.Route (TaskRoutes(..), TaskTrack(..))
import Flight.Track.Tag (Tagging(..))
import Flight.Track.Cross (Crossing)
import Flight.Track.Time (TimeRow(..), TickRow(..))
import Flight.Track.Mask (Masking)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , MaskTrackFile(..)
    , CompSettings(..)
    )

readComp :: CompInputFile -> ExceptT String IO CompSettings
readComp (CompInputFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

writeComp :: CompInputFile -> CompSettings -> IO ()
writeComp (CompInputFile path) compInput = do
    let cfg = Y.setConfCompare (fieldOrder compInput) Y.defConfig
    let yaml = Y.encodePretty cfg compInput
    BS.writeFile path yaml

readRoute :: TaskLengthFile -> ExceptT String IO TaskRoutes
readRoute (TaskLengthFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

writeRoute :: TaskLengthFile -> [Maybe TaskTrack] -> IO ()
writeRoute (TaskLengthFile lenPath) os = 
    BS.writeFile lenPath yaml
    where
        taskLength = TaskRoutes { taskRoutes = os }
        cfg = Y.setConfCompare (fieldOrder taskLength) Y.defConfig
        yaml = Y.encodePretty cfg taskLength

readCrossing :: CrossZoneFile -> ExceptT String IO Crossing
readCrossing (CrossZoneFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

writeCrossing :: CrossZoneFile -> Crossing -> IO ()
writeCrossing (CrossZoneFile path) crossZone = do
    let cfg = Y.setConfCompare (fieldOrder crossZone) Y.defConfig
    let yaml = Y.encodePretty cfg crossZone
    BS.writeFile path yaml

readTagging :: TagZoneFile -> ExceptT String IO Tagging
readTagging (TagZoneFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

writeTagging :: TagZoneFile -> Tagging -> IO ()
writeTagging (TagZoneFile path) tagZone = do
    let cfg = Y.setConfCompare (fieldOrder tagZone) Y.defConfig
    let yaml = Y.encodePretty cfg tagZone
    BS.writeFile path yaml

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

writeMasking :: MaskTrackFile -> Masking -> IO ()
writeMasking (MaskTrackFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

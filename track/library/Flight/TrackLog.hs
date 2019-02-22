{-|
Module      : Flight.TrackLog
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Competition pilot tracks logs.
-}
module Flight.TrackLog
    ( pilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    ) where

import Prelude hiding (readFile)
import Data.ByteString.UTF8 (toString)
import Data.ByteString (readFile)
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime(..), diffUTCTime, addUTCTime)
import Data.Time.Calendar
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (nubBy)
import Data.Char (toLower)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
    ( FilePath
    , (</>)
    , takeDirectory
    , normalise
    , splitDirectories
    , joinPath
    , takeExtension
    )

import qualified Flight.Kml as K
import qualified Flight.Igc as I (parse)
import Flight.Igc
    ( Degree(..), Minute(..), Second(..)
    , Year(..), Month(..), Day(..), Hour(..), HMS(..)
    , Lat(..), Lng(..), Altitude(..), AltGps(..), AltBaro(..)
    , IgcRecord(..)
    , isMark, isFix, addHoursIgc
    )
import Flight.Comp
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TrackFileFail(..)
    , TaskFolder(..)
    , IxTask(..)
    )
import Flight.Track.Range (asRollovers)

ixTasks :: [IxTask]
ixTasks = IxTask <$> [ 1 .. ]

pilotTrack
    :: (K.MarkedFixes -> a)
    -> PilotTrackLogFile
    -> ExceptT
        (Pilot, TrackFileFail)
        IO
        (Pilot, a)
pilotTrack _ (PilotTrackLogFile p Nothing) =
    ExceptT . return $ Left (p, TrackLogFileNotSet)
pilotTrack f (PilotTrackLogFile p (Just (TrackLogFile file))) = do
    let folder = takeDirectory file
    dde <- lift $ doesDirectoryExist folder
    x <- lift $
            if not dde
                then
                    return . Left $ TaskFolderExistsNot folder
                else do
                    dfe <- doesFileExist file
                    if not dfe
                        then return . Left $ TrackLogFileExistsNot file
                        else do
                            contents <- toString <$> readFile file

                            kml :: Either String K.MarkedFixes
                                <- case toLower <$> takeExtension file of
                                      ".kml" ->
                                          K.parse contents

                                      ".igc" ->
                                          case I.parse contents of
                                            Left _ ->
                                                return . Left
                                                $ "Can't parse IGC: " ++ file

                                            Right xs ->
                                                return . Right $ igcMarkedFixes xs

                                      _ ->
                                          K.parse contents

                            return $ bimap TrackLogFileNotRead f kml

    ExceptT . return . bimap (p,) (p,) $ x

taskPilotTracks
    :: (IxTask -> K.MarkedFixes -> a)
    -> [ (IxTask, [ PilotTrackLogFile ]) ]
    -> IO
        [[ Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]
taskPilotTracks _ [] =
    return []
taskPilotTracks f xs =
    sequence $ (\(i, ts) ->
        sequence $ runExceptT . pilotTrack (f i) <$> ts)
        <$> xs

pilotTracks
    :: (IxTask -> K.MarkedFixes -> a)
    -> [[ PilotTrackLogFile ]]
    -> IO
        [[ Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]
pilotTracks _ [] = return []
pilotTracks f tasks =
    taskPilotTracks f (zip ixTasks tasks) 

filterPilots
    :: [ Pilot ]
    -> [[ PilotTrackLogFile ]]
    -> [[ PilotTrackLogFile ]]

filterPilots [] xs = xs
filterPilots pilots xs =
    f <$> xs
    where
        f :: [ PilotTrackLogFile ] -> [ PilotTrackLogFile ]
        f ys =
            catMaybes
            $ (\x@(PilotTrackLogFile pilot _) ->
                if pilot `elem` pilots then Just x else Nothing)
            <$> ys

filterTasks
    :: [ IxTask ]
    -> [[ PilotTrackLogFile ]]
    -> [[ PilotTrackLogFile ]]

filterTasks [] xs = xs
filterTasks tasks xs =
    zipWith (\i ys ->
        if i `elem` tasks then ys else []) ixTasks xs

makeAbsolute
    :: FilePath
    -> TaskFolder
    -> PilotTrackLogFile
    -> PilotTrackLogFile
makeAbsolute _ _ x@(PilotTrackLogFile _ Nothing) = x
makeAbsolute
    dir
    (TaskFolder pathParts)
    (PilotTrackLogFile p (Just (TrackLogFile file))) =
    PilotTrackLogFile p (Just (TrackLogFile path))
    where
        parts :: [ FilePath ]
        parts = splitDirectories dir ++ pathParts

        path :: FilePath
        path = normalise $ joinPath parts </> file

nullMarkedFixes :: K.MarkedFixes
nullMarkedFixes = K.MarkedFixes (UTCTime (ModifiedJulianDay 0) 0) []

igcEqOrEqOnTime :: IgcRecord -> IgcRecord -> Bool
igcEqOrEqOnTime (B t0 _ _ _ _) (B t1 _ _ _ _) = t0 == t1
igcEqOrEqOnTime a b = a == b

-- | The B record only records time of day. If the sequence is not increasing
-- then for every rollback add a bump 24 hrs.
--
-- >>> asRollovers [7,9,2,3]
-- [[7,9],[2,3]]
bumpOverIgc :: [Flight.Igc.IgcRecord] -> [Flight.Igc.IgcRecord]
bumpOverIgc xs =
    bumpOver
        (flip $ addHoursIgc . Hour . show)
        [0 :: Integer, 24..]
        xs

-- | Apply a bump from a list every time there is a roll over.
--
-- >>> bumpOver (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,21]
bumpOver :: Ord a => (a -> b -> a) -> [b] -> [a] -> [a]
bumpOver add ns xs =
    concat
    [ (`add` n) <$> ys
    | ys <- asRollovers xs
    | n <- ns
    ]

-- |
-- >>> line 1 igcScott
-- "HFDTE080417\n"
igcMarkedFixes :: [Flight.Igc.IgcRecord] -> K.MarkedFixes
igcMarkedFixes xs =
    maybe nullMarkedFixes (`mark` zs) date
    where
        date =
            listToMaybe
            . take 1
            . filter isMark
            $ xs

        ys = bumpOverIgc $ filter isFix xs

        -- NOTE: Some loggers will be using sub-second logging. The columns in
        -- the B record holding the s or ss, tenths or hundredths of a second,
        -- are specified in the I record. Whether parsing IGC files at the
        -- second or sub-second granularity, we need to avoid having fixes with
        -- identical time stamps hence the nubBy here.
        zs = nubBy igcEqOrEqOnTime ys

mark :: IgcRecord -> [IgcRecord] -> K.MarkedFixes
mark Ignore _ = nullMarkedFixes
mark B{} _ = nullMarkedFixes
mark (HFDTEDATE (Day dd) (Month mm) (Year yy) _) xs =
    unStamp Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = stamp (dd, mm, yy) <$> ys
mark (HFDTE (Day dd) (Month mm) (Year yy)) xs =
    unStamp Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = stamp (dd, mm, yy) <$> ys

extract :: IgcRecord -> Maybe (HMS, (Lat, Lng, AltBaro, Maybe AltGps))
extract Ignore = Nothing
extract HFDTEDATE{} = Nothing
extract HFDTE{} = Nothing
extract (B hms lat lng alt altGps) = Just (hms, (lat, lng, alt, altGps))

-- | Combines date with time of day to get a @UTCTime@.
-- >>> stamp ("08", "07", "17") ((HMS (Hour "02") (Minute "37") (Second "56")), "")
-- (2017-07-08 02:37:56 UTC,"")
-- >>> stamp ("08", "07", "17") ((HMS (Hour "26") (Minute "37") (Second "56")), "")
-- (2017-07-09 02:37:56 UTC,"")
stamp :: (String, String, String) -> (HMS, a) -> (UTCTime, a)
stamp (dd, mm, yy) (HMS (Hour hr) (Minute minute) (Second sec), a) =
    (utc, a)
    where
        -- TODO: Test with an IGC file from the 20th Century.
        y = read ("20" ++ yy) :: Integer
        m = read mm :: Int
        d = read dd :: Int
        hr' = read hr :: Integer
        minute' = read minute :: Integer
        sec' = read sec :: Integer
        utc =
            (fromInteger $ 60 * ((60 * hr') + minute') + sec')
            `addUTCTime`
            (UTCTime (fromGregorian y m d) 0)

unStamp
    :: Maybe UTCTime
    -> [(UTCTime, (Lat, Lng, AltBaro, Maybe AltGps))]
    -> K.MarkedFixes
unStamp _ [] = nullMarkedFixes
unStamp Nothing xs@((t, _) : _) = unStamp (Just t) xs
unStamp (Just mark0) xs =
    K.MarkedFixes
        { K.mark0 = mark0
        , K.fixes = toFix mark0 <$> xs
        }

toFix :: UTCTime -> (UTCTime, (Lat, Lng, AltBaro, Maybe AltGps)) -> K.Fix
toFix mark0 (t, (lat, lng, altBaro, altGps)) =
    K.Fix
        { K.fixMark = K.Seconds . round $ t `diffUTCTime` mark0
        , K.fix =
            K.LLA
                { K.llaLat = readLat lat
                , K.llaLng = readLng lng
                , K.llaAltGps = readAltBaro altBaro
                }
        -- TODO: Which is Maybe GPS or BARO, KML vs IGC?
        , K.fixAltBaro = readAltGps <$> altGps
        }

readDegMin :: Degree -> Minute -> Rational
readDegMin (Degree d) (Minute m) =
    d' % 1 + toRational m' / 60000
    where
        d' = read d :: Integer
        m' = read m :: Double

readLat :: Lat -> K.Latitude
readLat (LatN d m) = K.Latitude $ readDegMin d m
readLat (LatS d m) = K.Latitude . negate $ readDegMin d m

readLng :: Lng -> K.Longitude
readLng (LngE d m) = K.Longitude $ readDegMin d m
readLng (LngW d m) = K.Longitude . negate $ readDegMin d m

readAltBaro :: AltBaro -> K.Altitude
readAltBaro (AltBaro (Altitude alt)) = K.Altitude (read alt :: Integer)

readAltGps :: AltGps -> K.Altitude
readAltGps (AltGps (Altitude alt)) = K.Altitude (read alt :: Integer)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Language.Haskell.TH
-- >>> import qualified Language.Haskell.TH.Syntax as TH (lift)
-- >>> import Flight.Igc
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = TH.lift =<< runIO readStr
-- :}
--
-- >>> line n = unlines . take 1 . drop n . lines
-- 
-- >>> fileScott  = "./test-suite-doctest/Scott-Barrett.20170409-071936.7601.19.igc"
--
-- >>> igcScott = $(embedStr (System.IO.readFile fileScott))


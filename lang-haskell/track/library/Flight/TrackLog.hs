{-# LANGUAGE BangPatterns #-}

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
    , pilotTrack
    , filterPilots
    , filterTasks
    , makeAbsolute
    ) where

import Prelude hiding (readFile)
import Data.ByteString.UTF8 (toString)
import Data.ByteString (readFile)
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime(..), diffUTCTime)
import Data.Time.Calendar
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char (toLower)
import Control.DeepSeq
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
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Flight.Kml as K
import qualified Flight.Igc as I (parse)
import Flight.Igc
    ( Degree(..), MinuteOfAngle(..)
    , Lat(..), Lng(..), Altitude(..), AltGps(..), AltBaro(..)
    , IgcRecord(..)
    , isMark, isFix, bumpOver
    )
import Flight.Comp
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TrackFileFail(..)
    , TaskFolder(..)
    , IxTask(..)
    )
import qualified Flight.Igc as Igc (mark)

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
                            contents <- readFile file

                            kml :: Either String K.MarkedFixes
                                <- case toLower <$> takeExtension file of
                                      ".kml" ->
                                          K.parse $ toString contents

                                      ".igc" ->
                                          case I.parse contents of
                                            Left _ ->
                                                return . Left
                                                $ "Can't parse IGC: " ++ file

                                            Right xs ->
                                                return . Right $!! igcMarkedFixes xs

                                      _ ->
                                          K.parse $ toString contents

                            return $ bimap TrackLogFileNotRead f kml

    ExceptT . return . bimap (p,) (p,) $ x
{-# SCC pilotTrack #-}

taskPilotTracks
    :: NFData a
    => (IxTask -> K.MarkedFixes -> a)
    -> [ (IxTask, [ PilotTrackLogFile ]) ]
    -> IO
        [[ Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]
taskPilotTracks _ [] =
    return []
taskPilotTracks f !xs =
    sequence
    $ (\(i, ts) -> do
        pts <- sequence $ runExceptT . pilotTrack (f i) <$> ts
        return $!! pts)
    <$> xs

pilotTracks
    :: NFData a
    => (IxTask -> K.MarkedFixes -> a)
    -> [[ PilotTrackLogFile ]]
    -> IO
        [[ Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]
pilotTracks _ [] = return []
pilotTracks f tasks =
    taskPilotTracks f (zip ixTasks tasks)
{-# INLINABLE pilotTracks #-}

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

igcMarkedFixes :: [Flight.Igc.IgcRecord] -> K.MarkedFixes
igcMarkedFixes !xs =
    maybe nullMarkedFixes (\d -> mark d $!! zs) date
    where
        date =
            listToMaybe
            . take 1
            . filter isMark
            $ xs

        xs' = {-# SCC filter_is_fix #-} filter isFix xs
        ys = {-# SCC bump_over #-} bumpOver xs'

        -- NOTE: Some loggers will be using sub-second logging. The columns in
        -- the B record holding the s or ss, tenths or hundredths of a second,
        -- are specified in the I record. Whether parsing IGC files at the
        -- second or sub-second granularity, we need to avoid having fixes with
        -- identical time stamps hence the nubBy here.
        zs =  {-# SCC nubOrdOn_hms #-} nubOrdOn hms ys
{-# SCC igcMarkedFixes #-}

-- TODO: Remove nubOrdOn and nubOrdOnExcluding when I can upgrade the
-- containers version.
nubOrdOn :: Ord b => (a -> b) -> [a] -> [a]
nubOrdOn f = \xs -> nubOrdOnExcluding f Set.empty xs
{-# INLINE nubOrdOn #-}

nubOrdOnExcluding :: Ord b => (a -> b) -> Set b -> [a] -> [a]
nubOrdOnExcluding f = go
  where
    go _ [] = []
    go s (x:xs)
      | fx `Set.member` s = go s xs
      | otherwise = x : go (Set.insert fx s) xs
      where !fx = f x

-- |
-- >>> mark markSasha fixesSasha
-- ((2018-01-03 03:13:00 UTC,2018-01-03 06:57:50 UTC),(00:00:00,03:44:50))
--
-- >>> mark markBrad fixesBrad
-- ((2018-01-03 04:05:47 UTC,2018-01-03 06:13:55 UTC),(00:00:00,02:08:08))
--
-- >>> mark markScott fixesScott
-- ((2017-04-08 02:37:56 UTC,2017-04-08 06:49:57 UTC),(00:00:00,04:12:01))
--
-- >>> mark markGordon fixesGordon
-- ((2018-01-02 00:44:29 UTC,2018-01-02 09:07:43 UTC),(00:00:00,08:23:14))
mark :: IgcRecord -> [IgcRecord] -> K.MarkedFixes
mark = Igc.mark unStamp
{-# INLINE mark #-}

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
{-# INLINE unStamp #-}

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
{-# INLINE toFix #-}

readDegMin :: Degree -> MinuteOfAngle -> Rational
readDegMin (Degree d) MinuteOfAngle{unThousandths} =
    d' % 1 + toRational m' / 60000
    where
        d' = fromIntegral d
        m' = fromIntegral unThousandths :: Integer
{-# INLINE readDegMin #-}

readLat :: Lat -> K.Latitude
readLat (LatN d m) = K.Latitude $ readDegMin d m
readLat (LatS d m) = K.Latitude . negate $ readDegMin d m
{-# INLINE readLat #-}

readLng :: Lng -> K.Longitude
readLng (LngE d m) = K.Longitude $ readDegMin d m
readLng (LngW d m) = K.Longitude . negate $ readDegMin d m
{-# INLINE readLng #-}

readAltBaro :: AltBaro -> K.Altitude
readAltBaro (AltBaro (Altitude alt)) = K.Altitude $ fromIntegral alt
{-# INLINE readAltBaro #-}

readAltGps :: AltGps -> K.Altitude
readAltGps (AltGps (Altitude alt)) = K.Altitude $ fromIntegral alt
{-# INLINE readAltGps #-}

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Test.QuickCheck
-- >>> import Data.List
-- >>> import Language.Haskell.TH
-- >>> import qualified Language.Haskell.TH.Syntax as TH (lift)
-- >>> import Flight.Igc (parse, markTicks, markTimes)
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = TH.lift =<< runIO readStr
-- :}
--
-- >>> line n = unlines . take 1 . drop n . lines
--
-- >>> fileSasha  = "./test-suite-doctest/Sasha-Serebrennikova.20180103-121306.30169.72.igc"
-- >>> fileBrad  = "./test-suite-doctest/Brad-Porter.20180104-095852.36822.34.igc"
-- >>> fileScott = "./test-suite-doctest/Scott-Barrett.20170409-071936.7601.19.igc"
-- >>> fileGordon = "./test-suite-doctest/Gordon_Rigg.20180103-111847.6433.8.igc"
--
-- >>> (markSasha : _, (fixesSasha, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileSasha)) in (partition isFix <$> partition isMark xs)
-- >>> (markBrad : _, (fixesBrad, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileBrad)) in (partition isFix <$> partition isMark xs)
-- >>> (markScott : _, (fixesScott, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileScott)) in (partition isFix <$> partition isMark xs)
-- >>> (markGordon : _, (fixesGordon, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileGordon)) in (partition isFix <$> partition isMark xs)

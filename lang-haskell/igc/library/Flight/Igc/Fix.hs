module Flight.Igc.Fix
    ( eqOnTime
    , bumpOver
    , extract
    , mark
    , markTimes
    , markTicks
    ) where

import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Bifunctor (first)
import Data.Maybe (catMaybes)
import Flight.Igc.Record
import Flight.Track.Range (asRollovers, asRolloversBy, deleteSort)

type Fix t a = (t, a)
type IgcFix = Fix HMS Pos

-- | Are two B records equal on time or are records of other types equal?
--
-- prop> eqOnTime x x == True
eqOnTime :: IgcRecord -> IgcRecord -> Bool
eqOnTime B{hms = t0} B{hms = t1} = t0 == t1
eqOnTime a b = a == b
{-# INLINABLE eqOnTime #-}

seconds :: HMS -> Second
seconds (HMS (Hour h) (MinuteOfTime m) (Second s)) =
    Second $ s + m * 60 + h * 3600

secondsDiff :: HMS -> HMS -> Second
secondsDiff t1 t0 =
    Second $ s1 - s0
    where
        Second s1 = seconds t1
        Second s0 = seconds t0

secondsCompare :: HMS -> HMS -> Ordering
secondsCompare t0 t1 =
    s0 `compare` s1
    where
        Second s0 = seconds t0
        Second s1 = seconds t1

-- | Are two B records increasing in time save for some allowable slippage?
slipFwdOnTime :: Second -> IgcRecord -> IgcRecord -> Ordering
slipFwdOnTime slip B{hms = t0} B{hms = t1} =
    if | secondsDiff t1 t0 > Second 0 -> LT
       | secondsDiff t0 t1 <= slip -> LT
       | otherwise -> secondsCompare t0 t1
slipFwdOnTime _ a b = a `compare` b

-- | The B record only records time of day. If the sequence is not increasing
-- then for every rollback a hour or more add a bump 24 hrs. Filter out any
-- rollback of less than one hour.
--
-- >>> asRollovers [7,9,2,3]
-- [[7,9],[2,3]]
--
-- >>> let xs = markTimes markJason fixesJason in take 4 xs
-- [2017-12-31 01:46:49 UTC,2017-12-31 01:46:45 UTC,2017-12-31 01:46:51 UTC,2017-12-31 01:46:52 UTC]
--
-- >>> :{
-- let f = _bumpOverBy (slipFwdOnTime $ Second 0) (flip $ addHoursIgc . Hour) [0 :: Int, 24..]
--     xs = markTimes markJason (f fixesJason)
-- in take 4 xs
-- :}
-- [2017-12-31 01:46:49 UTC,2018-01-01 01:46:45 UTC,2018-01-01 01:46:51 UTC,2018-01-01 01:46:52 UTC]
--
-- >>> :{
-- let f = _bumpOverBy (slipFwdOnTime $ Second 4) (flip $ addHoursIgc . Hour) [0 :: Int, 24..]
--     xs = markTimes markJason (f fixesJason)
-- in take 4 xs
-- :}
-- [2017-12-31 01:46:49 UTC,2017-12-31 01:46:45 UTC,2017-12-31 01:46:51 UTC,2017-12-31 01:46:52 UTC]
--
-- >>> :{
-- let f = _bumpOverBy (slipFwdOnTime $ Second 3) (flip $ addHoursIgc . Hour) [0 :: Int, 24..]
--     xs = markTimes markJason (f fixesJason)
-- in take 4 xs
-- :}
-- [2017-12-31 01:46:49 UTC,2018-01-01 01:46:45 UTC,2018-01-01 01:46:51 UTC,2018-01-01 01:46:52 UTC]
--
-- >>> :{
-- let f = deleteSort . _bumpOverBy (slipFwdOnTime $ Second 4) (flip $ addHoursIgc . Hour) [0 :: Int, 24..]
--     xs = markTimes markJason (f fixesJason)
-- in take 4 xs
-- :}
-- [2017-12-31 01:46:49 UTC,2017-12-31 01:46:51 UTC,2017-12-31 01:46:52 UTC,2017-12-31 01:46:53 UTC]
bumpOver :: [IgcRecord] -> [IgcRecord]
bumpOver xs =
    deleteSort $
    _bumpOverBy (slipFwdOnTime $ Second 3599)
        (flip $ addHoursIgc . Hour)
        [0 :: Int, 24..]
        xs

-- | Apply a bump from a list every time there is a roll over.
--
-- >>> _bumpOver (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,21]
_bumpOver :: Ord a => (a -> b -> a) -> [b] -> [a] -> [a]
_bumpOver add ns xs =
    concat
    [ (`add` n) <$> ys
    | ys <- asRollovers xs
    | n <- ns
    ]

-- |
-- >>> _bumpOverBy compare (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,21]
--
-- >>> _bumpOverBy (flip compare) (+) [0,10..] [7,9,2,3,1]
-- [7,19,12,23,21]
--
-- >>> _bumpOverBy (\a b -> a `compare` (b + 1)) (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,21]
--
-- >>> _bumpOverBy (\a b -> a `compare` (b + 2)) (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,11]
_bumpOverBy
    :: (a -> a -> Ordering)
    -> (a -> b -> a)
    -> [b]
    -> [a]
    -> [a]
_bumpOverBy p add ns xs =
    concat
    [ (`add` n) <$> ys
    | ys <- asRolloversBy p xs
    | n <- ns
    ]

-- | The B records have time of day in seconds. This function combines date
-- with time of day, marking each fix with a date and time.
mark
    :: Monoid b
    => (Maybe a -> [(UTCTime, Pos)] -> b)
    -> IgcRecord
    -> [IgcRecord]
    -> b
mark _ Ignore _ = mempty
mark _ G{} _ = mempty
mark _ B{} _ = mempty
mark f HFDTEDATE{ymd = YMD{year = yy, month = mm, day = dd}} xs =
    f Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = [stamp (yy, mm, dd) `first` y | y <- ys]
mark f (HFDTE YMD{year = yy, month = mm, day = dd}) xs =
    f Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = [stamp (yy, mm, dd) `first` y | y <- ys]
{-# INLINABLE mark #-}

-- | Extracts B record data as type @IgcFix@.
--
-- prop> not (isFix x) == (extract x == Nothing)
-- prop> isFix x == (extract x /= Nothing)
extract :: IgcRecord -> Maybe IgcFix
extract Ignore = Nothing
extract G{} = Nothing
extract HFDTEDATE{} = Nothing
extract HFDTE{} = Nothing
extract B{hms, pos} = Just (hms, pos)

-- | Combines date with time of day to get a @UTCTime@.
-- >>> stamp (Year 17, Month 7, Day 8) (HMS (Hour 2) (MinuteOfTime 37) (Second 56))
-- 2017-07-08 02:37:56 UTC
--
-- >>> stamp (Year 17, Month 7, Day 8) (HMS (Hour 26) (MinuteOfTime 37) (Second 56))
-- 2017-07-09 02:37:56 UTC
stamp :: (Year, Month, Day) -> HMS -> UTCTime
stamp (Year yy, Month mm, Day dd) (HMS (Hour hr) (MinuteOfTime minute) (Second sec)) =
    utc
    where
        -- TODO: Test with an IGC file from the 20th Century.
        y = 2000 + fromIntegral yy :: Integer
        hr' = fromIntegral hr
        minute' = fromIntegral minute
        sec' = fromIntegral sec
        utc =
            fromInteger $ 60 * ((60 * hr') + minute') + sec'
            `addUTCTime`
            UTCTime (fromGregorian y mm dd) 0

-- TODO: Why are Scott's tracklog and Jason's tracklog not sorted when marked
-- for time or ticks?

-- |
-- >>> let xs = markTimes markSasha fixesSasha in xs == sort xs
-- True
--
-- >>> let xs = markTimes markBrad fixesBrad in xs == sort xs
-- True
--
-- >>> let xs = markTimes markScott fixesScott in xs == sort xs
-- False
--
-- >>> let xs = markTimes markGordon fixesGordon in xs == sort xs
-- True
--
-- >>> let xs = markTimes markJason fixesJason in xs == sort xs
-- False
--
-- >>> let xs = markTimes markSasha fixesSasha in (head xs, head $ reverse xs)
-- (2018-01-03 03:12:59 UTC,2018-01-03 06:57:50 UTC)
--
-- >>> let xs = markTimes markBrad fixesBrad in (head xs, head $ reverse xs)
-- (2018-01-03 04:05:47 UTC,2018-01-03 06:13:55 UTC)
--
-- >>> let xs = markTimes markScott fixesScott in (head xs, head $ reverse xs)
-- (2017-04-08 02:37:56 UTC,2017-04-08 06:49:57 UTC)
--
-- >>> let xs = markTimes markGordon fixesGordon in (head xs, head $ reverse xs)
-- (2018-01-02 00:44:29 UTC,2018-01-02 09:07:43 UTC)
--
-- >>> let xs = markTimes markJason fixesJason in (head xs, head $ reverse xs)
-- (2017-12-31 01:46:49 UTC,2017-12-31 08:14:12 UTC)
--
-- >>> let xs = markTimes markJeff fixesJeff in (head xs, head $ reverse xs)
-- (2016-05-09 18:09:04 UTC,2016-05-09 20:00:35 UTC)
markTimes :: IgcRecord -> [IgcRecord] -> [UTCTime]
markTimes = mark unStampTime

-- |
-- >>> let xs = markTicks markSasha fixesSasha in xs == sort xs
-- True
--
-- >>> let xs = markTicks markBrad fixesBrad in xs == sort xs
-- True
--
-- >>> let xs = markTicks markScott fixesScott in xs == sort xs
-- False
--
-- >>> let xs = markTicks markGordon fixesGordon in xs == sort xs
-- True
--
-- >>> let xs = markTicks markJason fixesJason in xs == sort xs
-- False
--
-- >>> let xs = markTicks markSasha fixesSasha in (head xs, head $ reverse xs)
-- (00:00:00,03:44:51)
--
-- >>> let xs = markTicks markBrad fixesBrad in (head xs, head $ reverse xs)
-- (00:00:00,02:08:08)
--
-- >>> let xs = markTicks markScott fixesScott in (head xs, head $ reverse xs)
-- (00:00:00,04:12:01)
--
-- >>> let xs = markTicks markGordon fixesGordon in (head xs, head $ reverse xs)
-- (00:00:00,08:23:14)
--
-- >>> let xs = markTicks markJason fixesJason in (head xs, head $ reverse xs)
-- (00:00:00,06:27:23)
--
-- >>> let xs = markTicks markJeff fixesJeff in (head xs, head $ reverse xs)
-- (00:00:00,01:51:31)
markTicks :: IgcRecord -> [IgcRecord] -> [Second]
markTicks = mark unStampTick

unStampTime
    :: Maybe UTCTime
    -> [(UTCTime, (Lat, Lng, AltBaro, Maybe AltGps))]
    -> [UTCTime]
unStampTime _ xs = fst <$> xs

unStampTick
    :: Maybe UTCTime
    -> [(UTCTime, (Lat, Lng, AltBaro, Maybe AltGps))]
    -> [Second]
unStampTick _ [] = []
unStampTick Nothing xs@((t, _) : _) = unStampTick (Just t) xs
unStampTick (Just mark0) xs = toFixTick mark0 . fst <$> xs

toFixTick :: UTCTime -> UTCTime -> Second
toFixTick mark0 t = Second . round $ t `diffUTCTime` mark0

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Test.QuickCheck
-- >>> import System.IO (readFile)
-- >>> import Data.List
-- >>> import Language.Haskell.TH
-- >>> import Language.Haskell.TH.Syntax (lift)
-- >>> import Flight.Igc.Parse (parse)
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = lift =<< runIO readStr
-- :}
--
-- >>> line n = unlines . take 1 . drop n . lines
--
-- >>> fileSasha  = "./test-suite-doctest/Sasha-Serebrennikova.20180103-121306.30169.72.igc"
-- >>> fileBrad  = "./test-suite-doctest/Brad-Porter.20180104-095852.36822.34.igc"
-- >>> fileScott = "./test-suite-doctest/Scott-Barrett.20170409-071936.7601.19.igc"
-- >>> fileGordon = "./test-suite-doctest/Gordon_Rigg.20180103-111847.6433.8.igc"
-- >>> fileJason = "./test-suite-doctest/Jason_Kath.20180101-000746.18332.30.igc"
-- >>> fileJeff = "./test-suite-doctest/T4.kannard.126.igc"
--
-- >>> (markSasha : _, (fixesSasha, _)) = let (Right xs) = parse $(embedStr (readFile fileSasha)) in (partition isFix <$> partition isMark xs)
-- >>> (markBrad : _, (fixesBrad, _)) = let (Right xs) = parse $(embedStr (readFile fileBrad)) in (partition isFix <$> partition isMark xs)
-- >>> (markScott : _, (fixesScott, _)) = let (Right xs) = parse $(embedStr (readFile fileScott)) in (partition isFix <$> partition isMark xs)
-- >>> (markGordon : _, (fixesGordon, _)) = let (Right xs) = parse $(embedStr (readFile fileGordon)) in (partition isFix <$> partition isMark xs)
-- >>> (markJason : _, (fixesJason, _)) = let (Right xs) = parse $(embedStr (readFile fileJason)) in (partition isFix <$> partition isMark xs)
-- >>> (markJeff : _, (fixesJeff, _)) = let (Right xs) = parse $(embedStr (readFile fileJeff)) in (partition isFix <$> partition isMark xs)

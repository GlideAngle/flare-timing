module Flight.Igc.Fix
    ( igcEqOrEqOnTime
    , igcBumpOver
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
import Flight.Track.Range (asRollovers)

type Fix t a = (t, a)
type IgcFix = Fix HMS Pos

-- |
-- prop> igcEqOrEqOnTime x x == True
igcEqOrEqOnTime :: IgcRecord -> IgcRecord -> Bool
igcEqOrEqOnTime B{hms = t0} B{hms = t1} = t0 == t1
igcEqOrEqOnTime a b = a == b

-- | The B record only records time of day. If the sequence is not increasing
-- then for every rollback add a bump 24 hrs.
--
-- >>> asRollovers [7,9,2,3]
-- [[7,9],[2,3]]
igcBumpOver :: [IgcRecord] -> [IgcRecord]
igcBumpOver xs =
    bumpOver
        (flip $ addHoursIgc . Hour)
        [0 :: Int, 24..]
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

-- | The B records have time of day in seconds. This function combines date
-- with time of day, marking each fix with a date and time.
mark
    :: Monoid b
    => (Maybe a -> [(UTCTime, Pos)] -> b)
    -> IgcRecord
    -> [IgcRecord]
    -> b
mark _ Ignore _ = mempty
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

-- | Extracts B record data as type @IgcFix@.
--
-- prop> not (isFix x) == (extract x == Nothing)
-- prop> isFix x == (extract x /= Nothing)
extract :: IgcRecord -> Maybe IgcFix
extract Ignore = Nothing
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
            (fromInteger $ 60 * ((60 * hr') + minute') + sec')
            `addUTCTime`
            (UTCTime (fromGregorian y mm dd) 0)

-- |
-- >>> let xs = markTimes markGordon fixesGordon in xs == sort xs
-- True
markTimes :: IgcRecord -> [IgcRecord] -> [UTCTime]
markTimes = mark unStampTime

-- |
-- >>> let xs = markTicks markGordon fixesGordon in xs == sort xs
-- True
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
--
-- >>> (markSasha : _, (fixesSasha, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileSasha)) in (partition isFix <$> partition isMark xs)
-- >>> (markBrad : _, (fixesBrad, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileBrad)) in (partition isFix <$> partition isMark xs)
-- >>> (markScott : _, (fixesScott, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileScott)) in (partition isFix <$> partition isMark xs)
-- >>> (markGordon : _, (fixesGordon, _)) = let (Right xs) = parse $(embedStr (System.IO.readFile fileGordon)) in (partition isFix <$> partition isMark xs)

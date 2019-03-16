module Flight.Igc.Fix
    ( igcEqOrEqOnTime
    , igcBumpOver
    , extract
    , mark
    ) where

import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (catMaybes)
import Flight.Igc.Record
import Flight.Track.Range (asRollovers)

type Fix t a = (t, a)
type Pos = (Lat, Lng, AltBaro, Maybe AltGps)
type IgcFix = Fix HMS Pos

igcEqOrEqOnTime :: IgcRecord -> IgcRecord -> Bool
igcEqOrEqOnTime (B t0 _ _ _ _) (B t1 _ _ _ _) = t0 == t1
igcEqOrEqOnTime a b = a == b

-- | The B record only records time of day. If the sequence is not increasing
-- then for every rollback add a bump 24 hrs.
--
-- >>> asRollovers [7,9,2,3]
-- [[7,9],[2,3]]
igcBumpOver :: [IgcRecord] -> [IgcRecord]
igcBumpOver xs =
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

mark
    :: Monoid b
    => (Maybe a -> [(UTCTime, Pos)] -> b)
    -> IgcRecord
    -> [IgcRecord]
    -> b
mark _ Ignore _ = mempty
mark _ B{} _ = mempty
mark f (HFDTEDATE (Day dd) (Month mm) (Year yy) _) xs =
    f Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = [stamp (dd, mm, yy) y | y <- ys]
mark f (HFDTE (Day dd) (Month mm) (Year yy)) xs =
    f Nothing ts
    where
        ys = catMaybes $ extract <$> xs
        ts = [stamp (dd, mm, yy) y | y <- ys]

-- |
-- prop> not (isFix x) == (extract x == Nothing)
-- prop> isFix x == (extract x /= Nothing)
extract :: IgcRecord -> Maybe IgcFix
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

-- $setup
-- >>> import Test.QuickCheck

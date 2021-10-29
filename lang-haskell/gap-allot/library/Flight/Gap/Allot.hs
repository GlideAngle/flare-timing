{-# LANGUAGE DuplicateRecordFields #-}
module Flight.Gap.Allot
    ( ArrivalFraction(..)
    , arrivalRankFraction
    , arrivalTimeFraction
    , PilotTime(..)
    , bestTime'
    , SpeedFraction(..)
    , speedFraction
    ) where

import Data.Ratio ((%))
import qualified Data.List as List (minimum)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Gap.Pilots (PilotsAtEss(..))
import Flight.Gap.Place.Arrival (ArrivalPlacing(..))
import Flight.Gap.Time.Arrival (ArrivalLag(..))
import Flight.Gap.Time.Best (BestTime(..))
import Flight.Gap.Time.Pilot (PilotTime(..))
import Flight.Gap.Fraction.Arrival (ArrivalFraction(..))
import Flight.Gap.Fraction.Speed (SpeedFraction(..))
import Flight.Gap.Equation (PowerExponent, powerFraction, arrivalTimePowerFraction)

-- | Given the placing and the number of pilots making the end of the speed
-- section, work out the arrival fraction.
--
-- >>> arrivalRankFraction (PilotsAtEss 1) (ArrivalPlacing 1)
-- ArrivalFraction (1 % 1)
-- >>> arrivalRankFraction (PilotsAtEss 100) (ArrivalPlacing 1)
-- ArrivalFraction (1 % 1)
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacing 2)
-- ArrivalFraction (36347 % 64000)
-- >>> fromRational $ 36347 % 64000
-- 0.567921875
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacing 3)
-- ArrivalFraction (2641 % 8000)
-- >>> fromRational $ 2641 % 8000
-- 0.330125
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacing 4)
-- ArrivalFraction (2909 % 12800)
-- >>> fromRational $ 2909 % 12800
-- 0.227265625
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacingEqual 3 2)
-- ArrivalFraction (27191 % 102400)
-- >>> fromRational $ 27191 % 102400
-- 0.265537109375
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacingEqual 2 3)
-- ArrivalFraction (2641 % 8000)
-- >>> fromRational $ 2641 % 8000
-- 0.330125
--
-- >>> arrivalRankFraction (PilotsAtEss 4) (ArrivalPlacingEqual 1 4)
-- ArrivalFraction (43873 % 102400)
-- >>> fromRational $ 43873 % 102400
-- 0.428447265625
arrivalRankFraction :: PilotsAtEss -> ArrivalPlacing -> ArrivalFraction

arrivalRankFraction (PilotsAtEss n) (ArrivalPlacing rank)
    | n <= 0 =
        ArrivalFraction 0
    | rank <= 0 =
        ArrivalFraction 0
    | rank > n =
        ArrivalFraction 0
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
            ac = 1 - ((rank - 1) % n)

arrivalRankFraction (PilotsAtEss n) (ArrivalPlacingEqual rankEqual count)
    | n <= 0 =
        ArrivalFraction 0
    | rankEqual <= 0 =
        ArrivalFraction 0
    | rankEqual > n =
        ArrivalFraction 0
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
            m = fromIntegral count
            rank = (sum $ take m (repeat rankEqual) ++ take m [0..]) % count
            ac = (1 % 1) - ((rank - 1 % 1) * (1 % n))

arrivalTimeFraction
    :: PilotsAtEss
    -> ArrivalLag (Quantity Double [u| h |])
    -> ArrivalFraction

arrivalTimeFraction (PilotsAtEss n) lag
    | n <= 0 = ArrivalFraction 0
    | otherwise = arrivalTimePowerFraction lag

bestTime'
    :: [PilotTime (Quantity Double [u| h |])]
    -> Maybe (BestTime (Quantity Double [u| h |]))
bestTime' [] = Nothing
bestTime' xs =
    Just . BestTime $ t
    where
        PilotTime t = List.minimum xs

-- |
-- >>> printf "%.3f" t1
-- 5.306
--
-- >>> printf "%.3f" t2
-- 5.324
--
-- >>> printf "%.3f" t3
-- 5.336
--
-- >>> printf "%.3f" t4
-- 5.632
--
-- >>> printf "%.3f" t5
-- 5.765
--
-- >>> printf "%.3f" t6
-- 5.811
--
-- >>> printf "%.3f" t7
-- 5.833
--
-- >>> printf "%.3f" t8
-- 6.046
--
-- >>> printf "%.3f" t9
-- 6.047
--
-- >>> printf "%.3f" t10
-- 6.201
--
-- >>> printf "%.3f" t11
-- 6.289
--
-- >>> printf "%.3f" t12
-- 6.471
--
-- >>> printf "%.3f" t13
-- 6.628
--
-- >>> printf "%.3f" t14
-- 6.633
--
-- >>> printf "%.3f" t15
-- 6.648
--
-- >>> printf "%.3f" t16
-- 6.718
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t1))
-- 1.000
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t2))
-- 0.961
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t3))
-- 0.945
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t4))
-- 0.728
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t5))
-- 0.659
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t6))
-- 0.636
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t7))
-- 0.626
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t8))
-- 0.531
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t9))
-- 0.531
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t10))
-- 0.468
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t11))
-- 0.433
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t12))
-- 0.365
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t13))
-- 0.309
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t14))
-- 0.308
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t15))
-- 0.303
--
-- >>> showFrac $ speedFraction (BestTime (MkQuantity t1)) (PilotTime (MkQuantity t16))
-- 0.279
speedFraction
    :: PowerExponent
    -> BestTime (Quantity Double [u| h |])
    -> PilotTime (Quantity Double [u| h |])
    -> SpeedFraction
speedFraction pe (BestTime (MkQuantity tMin)) (PilotTime (MkQuantity t)) =
    SpeedFraction . toRational $ powerFraction pe tMin t

-- $setup
-- >>> import Text.Printf (printf)
--
-- >>> hmsToHH hh mm ss = let secs = fromIntegral $ ss + mm * 60 + hh * 3600 in secs / 3600
-- >>> showFrac (SpeedFraction x) = printf "%.3f" $ fromRational x
--
-- These are the times for the pilots making goal at Forbes 2018, task 4.
-- >>> t1 = hmsToHH 5 18 23
-- >>> t2 = hmsToHH 5 19 27
-- >>> t3 = hmsToHH 5 20 11
-- >>> t4 = hmsToHH 5 37 57
-- >>> t5 = hmsToHH 5 45 55
-- >>> t6 = hmsToHH 5 48 41
-- >>> t7 = hmsToHH 5 50  0
-- >>> t8 = hmsToHH 6  2 44
-- >>> t9 = hmsToHH 6  2 49
-- >>> t10 = hmsToHH 6 12  3
-- >>> t11 = hmsToHH 6 17 19
-- >>> t12 = hmsToHH 6 28 16
-- >>> t13 = hmsToHH 6 37 42
-- >>> t14 = hmsToHH 6 37 58
-- >>> t15 = hmsToHH 6 38 51
-- >>> t16 = hmsToHH 6 43  3

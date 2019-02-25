{-# LANGUAGE DuplicateRecordFields #-}
module Flight.Gap.Allot
    ( ArrivalFraction(..)
    , arrivalFraction
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
import Flight.Gap.Time.Best (BestTime(..))
import Flight.Gap.Time.Pilot (PilotTime(..))
import Flight.Gap.Ratio.Arrival (ArrivalFraction(..))
import Flight.Gap.Ratio.Speed (SpeedFraction(..))
import Flight.Gap.Equation (powerFraction)

-- | Given the placing and the number of pilots making the end of the speed
-- section, work out the arrival fraction.
--
-- >>> arrivalFraction (PilotsAtEss 1) (ArrivalPlacing 1)
-- ArrivalFraction (1 % 1)
-- >>> arrivalFraction (PilotsAtEss 100) (ArrivalPlacing 1)
-- ArrivalFraction (1 % 1)
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacing 2)
-- ArrivalFraction (36347 % 64000)
-- >>> fromRational $ 36347 % 64000
-- 0.567921875
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacing 3)
-- ArrivalFraction (2641 % 8000)
-- >>> fromRational $ 2641 % 8000
-- 0.330125
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacing 4)
-- ArrivalFraction (2909 % 12800)
-- >>> fromRational $ 2909 % 12800
-- 0.227265625
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacingEqual 3 2)
-- ArrivalFraction (27191 % 102400)
-- >>> fromRational $ 27191 % 102400
-- 0.265537109375
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacingEqual 2 3)
-- ArrivalFraction (2641 % 8000)
-- >>> fromRational $ 2641 % 8000
-- 0.330125
--
-- >>> arrivalFraction (PilotsAtEss 4) (ArrivalPlacingEqual 1 4)
-- ArrivalFraction (43873 % 102400)
-- >>> fromRational $ 43873 % 102400
-- 0.428447265625
arrivalFraction :: PilotsAtEss -> ArrivalPlacing -> ArrivalFraction

arrivalFraction (PilotsAtEss n) (ArrivalPlacing rank)
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

arrivalFraction (PilotsAtEss n) (ArrivalPlacingEqual rankEqual count)
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

bestTime'
    :: [PilotTime (Quantity Double [u| h |])]
    -> Maybe (BestTime (Quantity Double [u| h |]))
bestTime' [] = Nothing
bestTime' xs =
    Just . BestTime $ t
    where
        PilotTime t = List.minimum xs

speedFraction
    :: BestTime (Quantity Double [u| h |])
    -> PilotTime (Quantity Double [u| h |])
    -> SpeedFraction
speedFraction (BestTime (MkQuantity tMin)) (PilotTime (MkQuantity t)) =
    SpeedFraction . toRational $ powerFraction tMin t

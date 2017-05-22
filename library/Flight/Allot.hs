{-# lANGUAGE PatternSynonyms #-}
module Flight.Allot
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , BestTime(..)
    , PilotTime(..)
    , SpeedFraction(..)
    , speedFraction
    , BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    ) where

import Data.Ratio ((%))
import Flight.Ratio (pattern (:%))

newtype PilotsAtEss = PilotsAtEss Integer deriving (Eq, Show)
newtype PositionAtEss = PositionAtEss Integer deriving (Eq, Show)
newtype ArrivalFraction = ArrivalFraction Rational deriving (Eq, Ord, Show)

newtype BestTime = BestTime Rational deriving (Eq, Ord, Show)
newtype PilotTime = PilotTime Rational deriving (Eq, Ord, Show)
newtype SpeedFraction = SpeedFraction Rational deriving (Eq, Ord, Show)

newtype BestDistance = BestDistance Rational deriving (Eq, Ord, Show)
newtype PilotDistance= PilotDistance Rational deriving (Eq, Ord, Show)
newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
    | n <= 0 =
        ArrivalFraction (0 % 1)
    | rank <= 0 =
        ArrivalFraction (0 % 1)
    | rank > n =
        ArrivalFraction (0 % 1)
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
        ac = (1 % 1) - ((rank - 1) % n)

speedFraction :: BestTime -> PilotTime -> SpeedFraction
speedFraction (BestTime best) (PilotTime t) =
    SpeedFraction $ max (0 % 1) sf
    where
        numerator = fromRational $ t - best :: Double
        denominator = (fromRational best) ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - (toRational frac)

linearFraction :: BestDistance -> PilotDistance -> LinearFraction
linearFraction (BestDistance (nb :% db)) (PilotDistance (np :% dp)) =
    LinearFraction $ (np * db) % (dp * nb)

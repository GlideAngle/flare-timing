{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Gap.Allot
    ( PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , BestTime(..)
    , PilotTime(..)
    , bestTime
    , SpeedFraction(..)
    , speedFraction
    ) where

import Control.Newtype (Newtype(..))
import Data.Ratio ((%))
import qualified Data.List as List (minimum)
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Flight.Gap.Pilots (PilotsAtEss(..))

-- | A 1-based rank of the pilot arrival at goal, 1st in is 1, 2nd is 2 etc.
newtype PositionAtEss = PositionAtEss Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype ArrivalFraction = ArrivalFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces ArrivalFraction where
    defdp _ = DecimalPlaces 8

instance Newtype ArrivalFraction Rational where
    pack = ArrivalFraction
    unpack (ArrivalFraction a) = a

newtype SpeedFraction = SpeedFraction Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces SpeedFraction where
    defdp _ = DecimalPlaces 8

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

-- | Best time for the task, units of hours.
newtype BestTime = BestTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces BestTime where
    defdp _ = DecimalPlaces 8

instance Newtype BestTime Rational where
    pack = BestTime
    unpack (BestTime a) = a

-- | Pilot time for the task, units of hours.
newtype PilotTime = PilotTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces PilotTime where
    defdp _ = DecimalPlaces 8

instance Newtype PilotTime Rational where
    pack = PilotTime
    unpack (PilotTime a) = a

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
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

bestTime :: [PilotTime] -> Maybe BestTime
bestTime [] = Nothing
bestTime xs = let PilotTime t = List.minimum xs in Just $ BestTime t

speedFraction :: BestTime -> PilotTime -> SpeedFraction
speedFraction (BestTime best) (PilotTime t) =
    SpeedFraction $ max (0 % 1) sf
    where
        numerator = fromRational $ t - best :: Double
        denominator = fromRational best ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - toRational frac

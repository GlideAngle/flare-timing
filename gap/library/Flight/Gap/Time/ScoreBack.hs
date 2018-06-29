{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Time.ScoreBack (ScoreBackTime(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

-- | ScoreBack time for the task, units of hours.
newtype ScoreBackTime a = ScoreBackTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| s |])
    => DefaultDecimalPlaces (ScoreBackTime q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| s |])
    => Newtype (ScoreBackTime q) q where
    pack = ScoreBackTime
    unpack (ScoreBackTime a) = a

instance (q ~ Quantity Double [u| s |]) => ToJSON (ScoreBackTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| s |]) => FromJSON (ScoreBackTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

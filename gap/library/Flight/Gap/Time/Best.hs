{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Time.Best (BestTime(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

-- | Best time for the task, units of hours.
newtype BestTime a = BestTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| h |])
    => DefaultDecimalPlaces (BestTime q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| h |])
    => Newtype (BestTime q) q where
    pack = BestTime
    unpack (BestTime a) = a

instance (q ~ Quantity Double [u| h |]) => ToJSON (BestTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| h |]) => FromJSON (BestTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

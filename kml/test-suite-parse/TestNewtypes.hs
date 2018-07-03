{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC

import Flight.Types
    ( Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    )

instance Monad m => SC.Serial m Seconds where
    series = cons1 Seconds

instance QC.Arbitrary Seconds where
    arbitrary = Seconds <$> arbitrary

instance Monad m => SC.Serial m Altitude where
    series = cons1 Altitude

instance QC.Arbitrary Altitude where
    arbitrary = Altitude <$> arbitrary

instance Monad m => SC.Serial m Latitude where
    series = cons1 Latitude

instance QC.Arbitrary Latitude where
    arbitrary = Latitude <$> arbitrary

instance Monad m => SC.Serial m Longitude where
    series = cons1 Longitude

instance QC.Arbitrary Longitude where
    arbitrary = Longitude <$> arbitrary

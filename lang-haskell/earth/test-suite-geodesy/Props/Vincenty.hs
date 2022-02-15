module Props.Vincenty
    ( VincentyTest(..)
    , distancePoint
    , distanceVincenty
    , distanceVincentyF
    ) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (span)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..))
import Flight.Distance (QTaskDistance, TaskDistance(..), PathDistance(..))
import Flight.Zone (Zone(..), center)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Earth.Ellipsoid (wgs84)
import Props.Zone (ZonesTest(..))
import Ellipsoid.Vincenty.Span (spanD, spanR)

newtype VincentyTest a =
    VincentyTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (VincentyTest a)

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (VincentyTest a) where
    series = decDepth $ VincentyTest <$> series

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (VincentyTest a) where
    arbitrary = VincentyTest <$> arbitrary

correctPoint :: [Zone Rational] -> QTaskDistance Rational [u| m |] -> Bool
correctPoint [] (TaskDistance (MkQuantity d)) = d == 0
correctPoint [_] (TaskDistance (MkQuantity d)) = d == 0
correctPoint [Cylinder xR x, Cylinder yR y] (TaskDistance (MkQuantity d))
    | x == y = (xR == yR && d == 0) || d > 0
    | otherwise = d > 0
correctPoint xs (TaskDistance (MkQuantity d))
    | all (== head ys) (tail ys) = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

distanceVincentyF :: VincentyTest Double -> Bool
distanceVincentyF (VincentyTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanD wgs84 x y

distanceVincenty :: VincentyTest Rational -> Bool
distanceVincenty (VincentyTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanR wgs84 x y

distancePoint :: ZonesTest Rational -> Bool
distancePoint (ZonesTest xs) =
    (\(PathDistance d _) -> correctPoint xs d)
    $ distancePointToPoint (spanR wgs84) xs

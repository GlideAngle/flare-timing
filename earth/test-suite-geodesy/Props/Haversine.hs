module Props.Haversine
    ( HaversineTest(..)
    , distancePoint
    , distanceHaversine
    , distanceHaversineF
    ) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (span)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (QTaskDistance, TaskDistance(..), PathDistance(..))
import Flight.Zone (Zone(..), center)
import Flight.Zone.Path (distancePointToPoint)
import Props.Zone (ZonesTest(..))
import Sphere.Span (spanD, spanR)

newtype HaversineTest a =
    HaversineTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (HaversineTest a)

instance (Monad m, SC.Serial m a) => SC.Serial m (HaversineTest a) where
    series = decDepth $ HaversineTest <$>
        cons4 (\xlat xlng ylat ylng ->
            ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
            , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
            ))

instance Arbitrary a => QC.Arbitrary (HaversineTest a) where
    arbitrary = HaversineTest <$> do
        xlat <- arbitrary
        xlng <- arbitrary
        ylat <- arbitrary
        ylng <- arbitrary
        return ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
               , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
               )

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

distanceHaversineF :: HaversineTest Double -> Bool
distanceHaversineF (HaversineTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanD x y

distanceHaversine :: HaversineTest Rational -> Bool
distanceHaversine (HaversineTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanR x y

distancePoint :: ZonesTest Rational -> Bool
distancePoint (ZonesTest xs) =
    (\(PathDistance d _) -> correctPoint xs d)
    $ distancePointToPoint spanR xs

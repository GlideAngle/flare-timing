module Props.Euclidean (EuclideanTest(..)) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

newtype EuclideanTest a =
    EuclideanTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (EuclideanTest a)

instance (Monad m, SC.Serial m a) => SC.Serial m (EuclideanTest a) where
    series = decDepth $ EuclideanTest <$>
        cons4 (\xlat xlng ylat ylng ->
            ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
            , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
            ))

instance Arbitrary a => QC.Arbitrary (EuclideanTest a) where
    arbitrary = EuclideanTest <$> do
        xlat <- arbitrary
        xlng <- arbitrary
        ylat <- arbitrary
        ylng <- arbitrary
        return ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
               , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
               )

module Props.Zone (ZonesTest(..)) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..))
import Flight.Zone (Radius(..), Incline(..), Bearing(..), Zone(..))

newtype ZoneTest a = ZoneTest (Zone a)
newtype ZonesTest a = ZonesTest [Zone a]

deriving instance
    (Real a, Fractional a, Show a, Show (LatLng a [u| rad |]))
    => Show (ZoneTest a)

deriving instance
    (Real a, Fractional a, Show a, Show (LatLng a [u| rad |]))
    => Show (ZonesTest a)

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (ZoneTest a) where
    series = decDepth $ ZoneTest <$>
        cons2 (\lat lng ->
            Point (LatLng (lat, lng)))

        \/ cons3 (\lat lng b ->
            Vector
                (Bearing $ MkQuantity b)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Cylinder
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons4 (\lat lng (SC.Positive r) i ->
            Conical
                (Incline $ MkQuantity i)
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Line
                Nothing
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            SemiCircle
                Nothing
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (ZonesTest a) where
    series = decDepth $ cons1 (\xs -> ZonesTest $ (\(ZoneTest x) -> x) <$> xs)

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (ZoneTest a) where
    arbitrary = ZoneTest <$> do
        lat <- arbitraryBoundedRandom
        lng <- arbitraryBoundedRandom
        let ll = LatLng (lat, lng)

        QC.frequency $
            zip
                [1, 1, 4, 1, 1, 1]
                [ return $ Point ll
                , do
                    (QC.Positive b) <- arbitrary
                    return $ Vector (Bearing $ MkQuantity b) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Cylinder (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    (QC.Positive i) <- arbitrary
                    return $ Conical (Incline $ MkQuantity i) (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Line Nothing (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ SemiCircle Nothing (Radius $ MkQuantity r) ll
                ]

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (ZonesTest a) where
    arbitrary = do
        len <- choose (2, 4)
        xs <- vector len
        return $ ZonesTest ((\(ZoneTest x) -> x) <$> xs)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Ellipsoid.Bedford (bedfordUnits) where

import Prelude hiding (min)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure ((-:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl (distanceVincenty)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import DegMinSec (DMS(..), toDeg)
import Bedford (points)

diff :: Num a => TaskDistance a -> TaskDistance a -> TaskDistance a
diff (TaskDistance a) (TaskDistance b) =
    TaskDistance $ a -: b

solutions :: [TaskDistance Double]
solutions =
    TaskDistance . convert <$>
    [ [u| 80.471341 km |]
    , [u| 80.467842 km |]
    , [u| 80.463616 km |]
    , [u| 80.468422 km |]
    , [u| 80.466106 km |]
    , [u| 80.463284 km |]
    , [u| 80.465497 km |]
    , [u| 80.464363 km |]
    , [u| 80.462951 km |]
    , [u| 80.466994 km |]
    , [u| 80.4659 km |]
    , [u| 80.463589 km |]
    , [u| 482.827311 km |]
    , [u| 482.805398 km |]
    , [u| 482.780699 km |]
    , [u| 482.810039 km |]
    , [u| 482.795399 km |]
    , [u| 482.778968 km |]
    , [u| 482.793074 km |]
    , [u| 482.786227 km |]
    , [u| 482.777777 km |]
    , [u| 804.711122 km |]
    , [u| 804.673374 km |]
    , [u| 804.633279 km |]
    , [u| 804.682723 km |]
    , [u| 804.657459 km |]
    , [u| 804.630834 km |]
    , [u| 804.655123 km |]
    , [u| 804.643847 km |]
    , [u| 804.629907 km |]
    , [u| 4828.136258 km |]
    , [u| 4827.891819 km |]
    , [u| 4827.781145 km |]
    , [u| 4828.022935 km |]
    , [u| 4827.861414 km |]
    , [u| 4827.797208 km |]
    , [u| 4827.933527 km |]
    , [u| 4827.899946 km |]
    , [u| 4827.858337 km |]
    ]

mmTolerance :: Quantity Double [u| mm |]
mmTolerance = [u| 1 % 2 mm |]

dblChecks :: [TaskDistance Double] -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
dblChecks slns pts =
    zipWith
        (\expected (x, y) ->
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show expected
                )
            $ diff (found x y) expected
            @?<= tolerance)
        slns
        pts
    where
        tolerance = TaskDistance . convert $ mmTolerance
        found x y = Dbl.distanceVincenty wgs84 (toLL x) (toLL y)

        toLL :: (DMS, DMS) -> LatLng Double [u| rad |]
        toLL (lat, lng) =
            LatLng (Lat lat'', Lng lng'')
                where
                    lat' :: Quantity Double [u| deg |]
                    lat' = MkQuantity . toDeg $ lat

                    lng' :: Quantity Double [u| deg |]
                    lng' = MkQuantity . toDeg $ lng

                    lat'' = convert lat' :: Quantity _ [u| rad |]
                    lng'' = convert lng' :: Quantity _ [u| rad |]

ratChecks :: [TaskDistance Double] -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
ratChecks slns pts =
    zipWith
        (\(TaskDistance d) (x, y) ->
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show (expected d)
                )
            $ diff (found x y) (expected d)
            @?<= tolerance)
        slns
        pts
    where
        tolerance = TaskDistance [u| 20 % 1000 m |]
        expected d = TaskDistance $ toRational' d
        found x y = Rat.distanceVincenty e wgs84 (toLL x) (toLL y)

        e = Epsilon $ 1 % 1000000000000000000
        toLL :: (DMS, DMS) -> LatLng Rational [u| rad |]
        toLL (lat, lng) =
            LatLng (Lat lat'', Lng lng'')
                where
                    lat' :: Quantity Rational [u| deg |]
                    lat' = toRational' . MkQuantity . toDeg $ lat

                    lng' :: Quantity Rational [u| deg |]
                    lng' = toRational' . MkQuantity . toDeg $ lng

                    lat'' = convert lat' :: Quantity _ [u| rad |]
                    lng'' = convert lng' :: Quantity _ [u| rad |]

bedfordUnits :: TestTree
bedfordUnits =
    testGroup ("Bedford Institute of Oceanography distances ± " ++ show mmTolerance)
    [ testGroup "with doubles" $ dblChecks solutions points
    , testGroup "with rationals" $ ratChecks solutions points
    ]

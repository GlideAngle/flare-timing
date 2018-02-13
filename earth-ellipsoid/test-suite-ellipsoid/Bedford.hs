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

-- | Test data from ...
-- 
-- Bedford Institute of Oceanography
-- Evaluation Direct and Inverse Geodetic Algorithms
-- Paul Delorme, September 1978.
module Bedford (bedfordUnits) where

import Prelude hiding (min)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (TaskDistance(..))
import qualified Flight.Ellipsoid.PointToPoint.Double as Dbl (distanceVincenty)
import Flight.Ellipsoid (wgs84)

newtype DMS = DMS (Int, Int, Double)

instance Show DMS where
    show = showDMS

showDMS :: DMS -> String
showDMS (DMS (deg, min, 0)) =
    show deg ++ "°" ++ show min ++ "'"
showDMS (DMS (deg, min, sec)) =
    show deg ++ "°" ++ show min ++ "'" ++ show sec ++ "''"

toDeg :: DMS -> Double
toDeg (DMS (deg, min, sec)) =
    fromIntegral deg + (fromIntegral min / 60) + (sec / 3600)

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

points :: [((DMS, DMS), (DMS, DMS))]
points =
    (\((xLat, xLng), (yLat, yLng)) -> ((DMS xLat, DMS xLng), (DMS yLat, DMS yLng)))
    <$>
    [ (((10, 0, 0.0), (-18, 0, 0.0)), ((10, 43, 39.078), (-18, 0, 0.0)))
    ]

solutions :: [TaskDistance Double]
solutions =
    TaskDistance . MkQuantity <$>
    [ 80466.478
    ]

checks :: [TaskDistance Double] -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
checks slns pts =
    zipWith
        (\d (x, y) ->
            HU.testCase (show x ++ " to " ++ show y)
            $ Dbl.distanceVincenty wgs84 (toLL x) (toLL y) @?= d)
        slns
        pts

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    $ checks solutions points

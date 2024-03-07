{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cylinder.Flat.Outer (outerUnits, outerUnitsR) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure
    ((*:), u, convert, unQuantity, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone (QBearing, Bearing(..))
import Flight.Zone.Cylinder (Tolerance(..))
import Cylinder.Flat.Span
    ( spanD, csD, spD
    , spanR, csR, spR
    , zpFilter
    )
import Cylinder.Outer (ptsUTM, distances, searchRanges, outerCheck)

outerUnits :: TestTree
outerUnits =
    testGroup "When points meant to be on the boundary are outside a cylinder"
        [ outerCheck spanD csD spD b t s zpFilter d p
        | p <- (\(x, y) -> (LatLng (Lat x, Lng $ plusMinusPi y))) <$> ptsUTM
        , b <- bearingD
        , (d, t, s) <-
            [ (d, t, s)
            | d <- distances
            | t <- Tolerance . unQuantity <$> tolerancesD
            | s <- searchRanges
            ]
        ]

outerUnitsR :: TestTree
outerUnitsR =
    testGroup "When points meant to be on the boundary are outside a cylinder"
        [ outerCheck spanR csR spR b t s zpFilter d p
        | p <- (\(x, y) -> (LatLng (Lat x, Lng $ plusMinusPi y))) <$> ptsUTM
        , b <- bearingR
        , (d, t, s) <-
            [ (d, t, s)
            | d <- distances
            | t <- Tolerance . unQuantity <$> tolerancesR
            | s <- searchRanges
            ]
        ]

tolerancesD :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesD =
    repeat $ fromRational' [u| 0.000001 mm |]

tolerancesR :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesR =
    repeat $ fromRational' [u| 0 mm |]

bearingD :: [QBearing Double [u| rad |]]
bearingD =
    Bearing <$>
    [ f $ x *: [u| 1 deg |]
    | x <- [0, 45 .. 360]
    ]
    where
        f :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
        f = convert

bearingR :: [QBearing Rational [u| rad |]]
bearingR =
    Bearing . toRational' <$>
    [ f $ x *: [u| 1 deg |]
    | x <- [0, 120]
    ]
    where
        f :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
        f = convert


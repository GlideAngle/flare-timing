{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Ellipsoid.Meridian (meridianUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure ((*:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Radius(..))
import Ellipsoid.Distance
import Zone

meridianUnits :: TestTree
meridianUnits =
    testGroup "Meridian arc distance tests"
    $ ((uncurry f) <$> describedZones)
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    meridianArc . convert
    <$> [ x *: [u| 1 deg |] | x <- [5, 10 .. 90]]
    where
        meridianArc d =
            (([u| 0 rad |], [u| 0 rad |]), (d, [u| 0 rad |]))

distances :: [Radius Rational [u| m |]]
distances =
    Radius
    <$>
    -- NOTE: These distances are not from Vincenty. They come from the first
    -- column of Table 3;
    -- "Comparison of series formulas for the Calculation of Meridian Arcs"
    --
    -- in the article;
    -- "New Meridian Arc formulas for Sailing Calculations in Navigational GIS"
    --
    -- in the journal;
    -- "International Hydrographic Review", May 2009.
    --
    -- SEE: https://journals.lib.unb.ca/index.php/ihr/article/view/20832
    -- 5°, 10°
    [ [u| 552885.45156 m |]
    , [u| 1105854.83418 m |]

    -- 15°, 20°
    , [u| 1658989.59067 m |]
    , [u| 2212366.25562 m |]

    -- 25°, 30°
    , [u| 2766054.17059 m |]
    , [u| 3320113.39921 m |]

    -- 35°, 40°
    , [u| 3874592.90264 m |]
    , [u| 4429529.03085 m |]

    -- 45°, 50°
    , [u| 4984944.37798 m |]
    , [u| 5540847.04118 m |]

    -- 55°, 60°
    , [u| 6097230.31218 m |]
    , [u| 6654072.81821 m |]

    -- 65°, 70°
    , [u| 7211339.11585 m |]
    , [u| 7768980.72630 m |]

    -- 75°, 80°
    , [u| 8326937.59000 m |]
    , [u| 8885139.87094 m |]

    -- 85°, 90°
    , [u| 9443510.14009 m |]
    , [u| 10001965.72922 m |]
    ]

tolerances :: [Quantity Rational [u| mm |]]
tolerances =
    -- 5°, 10°
    [ [u| 1.0 mm |]
    , [u| 1.0 mm |]

    -- 15°, 20°
    , [u| 1.5 mm |]
    , [u| 1.5 mm |]

    -- 25°, 30°
    , [u| 1.5 mm |]
    , [u| 1.5 mm |]

    -- 35°, 40°
    , [u| 1.0 mm |]
    , [u| 0.5 mm |]

    -- 45°, 50°
    , [u| 0.5 mm |]
    , [u| 0.5 mm |]

    -- 55°, 60°
    , [u| 1.0 mm |]
    , [u| 1.5 mm |]

    -- 65°, 70°
    , [u| 1.5 mm |]
    , [u| 1.5 mm |]

    -- 75°, 80°
    , [u| 3.0 mm |]
    , [u| 1.5 mm |]

    -- 85°, 90°
    , [u| 1.0 mm |]
    , [u| 0.5 mm |]
    ]

distanceMeridian
    :: String
    -> MkZone Double
    -> TestTree
distanceMeridian s f =
    testGroup s
    $ zipWith3
        (\tolerance r@(Radius r') (x, y) ->
            toDistanceClose
                tolerance
                r'
                (showQ x ++ " " ++ showQ y)
                (f r x, f r y))
        tolerances
        distances
        (pts :: [(QLL Double, QLL Double)])

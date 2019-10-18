module Meridian.Ellipsoid.Vincenty (units, unitsR) where

import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure ((*:), u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (QRadius, Radius(..))
import Zone (MkZone, QLL, describedZones, showQ)
import qualified Distance as D (DistanceClose, toDistanceClose)
import Flight.Earth.Ellipsoid (wgs84)
import Ellipsoid.Vincenty.Span (spanD, spanR)

units :: TestTree
units =
    testGroup "Meridian arc distance tests"
    [testGroup "With doubles" (uncurry f <$> describedZones)]
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose $ spanD wgs84)
                tolerances

unitsR :: TestTree
unitsR =
    testGroup "Meridian arc distance tests"
    [testGroup "With rationals" (uncurry f <$> describedZones)]
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose $ spanR wgs84)
                tolerances

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    meridianArc . convert
    <$> [ x *: [u| 1 deg |] | x <- [5, 10 .. 90]]
    where
        meridianArc d =
            (([u| 0 rad |], [u| 0 rad |]), (d, [u| 0 rad |]))

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    Radius . fromRational'
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

tolerances :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerances =
    -- 5°, 10°
    [ [u| 0.51 mm |]
    , [u| 1.0 mm |]

    -- 15°, 20°
    , [u| 1.3 mm |]
    , [u| 1.5 mm |]

    -- 25°, 30°
    , [u| 1.5 mm |]
    , [u| 1.3 mm |]

    -- 35°, 40°
    , [u| 0.95 mm |]
    , [u| 0.50 mm |]

    -- 45°, 50°
    , [u| 0.0014 mm |]
    , [u| 0.51 mm |]

    -- 55°, 60°
    , [u| 0.95 mm |]
    , [u| 1.3 mm |]

    -- 65°, 70°
    , [u| 1.5 mm |]
    , [u| 1.5 mm |]

    -- 75°, 80°
    , [u| 2.8 mm |]
    , [u| 1.0 mm |]

    -- 85°, 90°
    , [u| 0.58 mm |]
    , [u| 0.092 mm |]
    ]

distanceMeridian
    :: (Enum a, Real a, Fractional a)
    => String
    -> D.DistanceClose a
    -> [Quantity a [u| mm |]]
    -> MkZone a a
    -> TestTree
distanceMeridian s f tolerances' g =
    testGroup s
    $ zipWith3
        (\tolerance r@(Radius r') (x, y) ->
                f
                tolerance
                r'
                (showQ x ++ " " ++ showQ y)
                (g r x, g r y))
        tolerances'
        distances
        pts

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test data from the Forbes PreWords
-- 
-- SEE: http://www.forbesflatlands.com/results-2012
module Forbes
    ( (.>=.)
    , (.~=.)
    , tasks
    , d1, d2, d3, d4, d5, d6, d7, d8
    , toLL
    , mkDayUnits
    , tdRound
    ) where

import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit ((@?), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone (Radius(..), Zone(..))
import Data.Number.RoundingFunctions (dpRound)

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.~=.) :: (Show a, Show b) => a -> b -> String
(.~=.) x y = show x ++ " ~= " ++ show y

tdRound :: TaskDistance Rational -> TaskDistance Rational
tdRound (TaskDistance (MkQuantity d)) =
    TaskDistance . MkQuantity . dpRound 2 $ d

-- | The input pair is in degrees while the output is in radians.
toLL :: (Double, Double) -> LatLng Rational [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' = (MkQuantity $ toRational lat) :: Quantity Rational [u| deg |]
            lng' = (MkQuantity $ toRational lng) :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]

mkDayUnits
    :: ([Zone Rational] -> PathDistance Rational)
    -> TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> [TaskDistance Rational]
    -> TestTree
mkDayUnits pp title pDay dDay' dsDay' = testGroup title
    [ testCase
        ("point-to-point distance >= " ++ show dDay)
        $ (ppDay >= dDay) @? ppDay .>=. dDay

    , testGroup
        ( "\n>=\n"
        ++ show ppDayInits
        ++ "\n"
        ++ show dsDay
        )
        [ testCase "point-to-point distances"
          $ (ppDayInits >= dsDay) @? ppDayInits .>=. dsDay
        ]
    ]
    where
        dDay = tdRound dDay'
        dsDay = tdRound <$> dsDay'

        ppDay :: TaskDistance Rational
        ppDay = tdRound . edgesSum $ pp pDay

        pDayInits :: [[Zone Rational]]
        pDayInits = drop 1 $ inits pDay

        ppDayInits :: [TaskDistance Rational]
        ppDayInits = tdRound . edgesSum . pp <$> pDayInits


tasks :: [[Zone Rational]]
tasks = [d1, d2, d3, d4, d5, d6, d7, d8]

d1 :: [Zone Rational]
d1 =
    [ Cylinder (Radius $ MkQuantity 100) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 10000) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.85373, 147.94195)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.4397, 148.34533)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.61965, 148.4099)
    ]

d2 :: [Zone Rational]
d2 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.12592, 147.91043))
    ]

d3 :: [Zone Rational]
d3 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.02107, 148.2233))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.11795, 148.5013))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.82197, 148.66543))
    ]

d4 :: [Zone Rational]
d4 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.46363, 148.989))
    ]

d5 :: [Zone Rational]
d5 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.56608, 148.22657))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.0164, 149.43363))
    ]

d6 :: [Zone Rational]
d6 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.19498, 147.76218))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 31.69323, 148.29623))
    ]

d7 :: [Zone Rational]
d7 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.76052, 148.64958))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.93585, 148.74947))
    ]

d8 :: [Zone Rational]
d8 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 33.75343, 147.52865))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.12908, 147.57323))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.361, 147.9315))
    ]

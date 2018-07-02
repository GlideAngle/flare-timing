{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Test data from the Forbes PreWords
-- 
-- SEE: http://www.forbesflatlands.com/results-2012
module Forbes
    ( (.>=.)
    , (.~=.)
    , tasks
    , d1, d2, d3, d4, d5, d6, d7, d8
    , p1, p2, p3, p4, p5, p6, p7, p8
    , toLL
    , mkDayUnits, mkPartDayUnits
    , tdRound
    ) where

import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit ((@?), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (TaskDistance(..), PathDistance(..), fromKms)
import Flight.Zone (Radius(..), Zone(..))
import Data.Ratio.Rounding (dpRound)

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.~=.) :: (Show a, Show b) => a -> b -> String
(.~=.) x y = show x ++ " ~= " ++ show y

tdRound :: TaskDistance Rational -> TaskDistance Rational
tdRound (TaskDistance (MkQuantity d)) =
    TaskDistance . MkQuantity . dpRound 3 $ d

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
    -> Quantity Rational [u| km |]
    -> [Quantity Rational [u| km |]]
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
        dDay = tdRound . fromKms $ dDay'
        dsDay = tdRound . fromKms <$> dsDay'

        ppDay :: TaskDistance Rational
        ppDay = tdRound . edgesSum $ pp pDay

        pDayInits :: [[Zone Rational]]
        pDayInits = drop 1 $ inits pDay

        ppDayInits :: [TaskDistance Rational]
        ppDayInits = tdRound . edgesSum . pp <$> pDayInits

mkPartDayUnits
    :: ([Zone Rational] -> PathDistance Rational)
    -> TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> TestTree
mkPartDayUnits pp title zs (TaskDistance d) = testGroup title
    [ testCase
        ( "point-to-point distance "
        ++ show td'
        ++ " ~= "
        ++ show tdR
        )
        $ (tdR' == tdR) @? tdR' .~=. tdR
    ]
    where
        dKm = convert d :: Quantity Rational [u| km |]
        Flip r = dpRound 6 <$> Flip dKm
        tdR = TaskDistance (convert r :: Quantity Rational [u| m |])

        td'@(TaskDistance d') = edgesSum $ pp zs
        dKm' = convert d' :: Quantity Rational [u| km |]
        Flip r' = dpRound 6 <$> Flip dKm'
        tdR' = TaskDistance (convert r' :: Quantity Rational [u| m |])

tasks :: [[Zone Rational]]
tasks = [d1, d2, d3, d4, d5, d6, d7, d8]

d1 :: [Zone Rational]
d1 =
    [ Cylinder (Radius $ MkQuantity 100) $ toLL (-33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 10000) $ toLL (-33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.85373, 147.94195)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.4397, 148.34533)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.61965, 148.4099)
    ]

d2 :: [Zone Rational]
d2 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.12592, 147.91043))
    ]

d3 :: [Zone Rational]
d3 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.02107, 148.2233))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.11795, 148.5013))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.82197, 148.66543))
    ]

d4 :: [Zone Rational]
d4 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (-32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.46363, 148.989))
    ]

d5 :: [Zone Rational]
d5 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.56608, 148.22657))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.0164, 149.43363))
    ]

d6 :: [Zone Rational]
d6 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.19498, 147.76218))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-31.69323, 148.29623))
    ]

d7 :: [Zone Rational]
d7 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.76052, 148.64958))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.93585, 148.74947))
    ]

d8 :: [Zone Rational]
d8 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-33.75343, 147.52865))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.12908, 147.57323))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.361, 147.9315))
    ]

type MkPart
    = TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> TestTree

pairs :: [a] -> ([a], [a], [a])
pairs xs = (s1, s2, s3)
    where
        s1 = take 2 xs
        s2 = take 2 $ drop 1 xs
        s3 = take 2 $ drop 2 xs


p1 :: MkPart -> _ -> _ -> _ -> TestTree
p1 mk t1 t2 t3 =
    testGroup "Task 1 [...]"
    [ mk "Task 1 [x, x, _, _]" s1 (fromKms t1)
    , mk "Task 1 [_, x, x, _]" s2 (fromKms t2)
    , mk "Task 1 [_, _, x, x]" s3 (fromKms t3)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-33.85373, 147.94195)
                , (-33.4397, 148.34533)
                , (-33.61965, 148.4099)
                ]

            (s1, s2, s3) = pairs xs

p2 :: MkPart -> _ -> _ -> _ -> TestTree
p2 mk t1 t2 t3 =
    testGroup "Task 2 [...]"
    [ mk "Task 2 [x, x, _, _]" s1 (fromKms t1)
    , mk "Task 2 [_, x, x, _]" s2 (fromKms t2)
    , mk "Task 2 [_, _, x, x]" s3 (fromKms t3)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-32.90223, 147.98492)
                , (-32.9536, 147.55457)
                , (-33.12592, 147.91043)
                ]

            (s1, s2, s3) = pairs xs

p3 :: MkPart -> _ -> _ -> _ -> TestTree
p3 mk t1 t2 t3 =
    testGroup "Task 3 [...]"
    [ mk "Task 3 [x, x, _, _]" s1 (fromKms t1)
    , mk "Task 3 [_, x, x, _]" s2 (fromKms t2)
    , mk "Task 3 [_, _, x, x]" s3 (fromKms t3)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-34.02107, 148.2233)
                , (-34.11795, 148.5013)
                , (-34.82197, 148.66543)
                ]

            (s1, s2, s3) = pairs xs

p4 :: MkPart -> _ -> _ -> TestTree
p4 mk t1 t2 =
    testGroup "Task 4 [...]"
    [ mk "Task 4 [x, x, _]" s1 (fromKms t1)
    , mk "Task 4 [_, x, x]" s2 (fromKms t2)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-32.90223, 147.98492)
                , (-32.46363, 148.989)
                ]

            (s1, s2, _) = pairs xs

p5 :: MkPart -> _ -> _ -> TestTree
p5 mk t1 t2 =
    testGroup "Task 5 [...]"
    [ mk "Task 5 [x, x, _]" s1 (fromKms t1)
    , mk "Task 5 [_, x, x]" s2 (fromKms t2)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-32.56608, 148.22657)
                , (-32.0164, 149.43363)
                ]

            s1 = take 2 xs
            s2 = take 2 $ drop 1 xs

p6 :: MkPart -> _ -> _ -> TestTree
p6 mk t1 t2 =
    testGroup "Task 6 [...]"
    [ mk "Task 6 [x, x, _]" s1 (fromKms t1)
    , mk "Task 6 [_, x, x]" s2 (fromKms t2)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-32.19498, 147.76218)
                , (-31.69323, 148.29623)
                ]

            (s1, s2, _) = pairs xs

p7 :: MkPart -> _ -> _ -> _ -> TestTree
p7 mk t1 t2 t3 =
    testGroup "Task 7 [...]"
    [ mk "Task 7 [x, x, _, _]" s1 (fromKms t1)
    , mk "Task 7 [_, x, x, _]" s2 (fromKms t2)
    , mk "Task 7 [_, _, x, x]" s3 (fromKms t3)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-32.9536, 147.55457)
                , (-32.76052, 148.64958)
                , (-32.93585, 148.74947)
                ]

            (s1, s2, s3) = pairs xs

p8 :: MkPart -> _ -> _ -> _ -> TestTree
p8 mk t1 t2 t3 =
    testGroup "Task 8 [...]"
    [ mk "Task 8 [x, x, _, _]" s1 (fromKms t1)
    , mk "Task 8 [_, x, x, _]" s2 (fromKms t2)
    , mk "Task 8 [_, _, x, x]" s3 (fromKms t3)
    ]
        where
            xs =
                Point . toLL <$>
                [ (-33.36137, 147.93207)
                , (-33.75343, 147.52865)
                , (-33.12908, 147.57323)
                , (-33.361, 147.9315)
                ]

            (s1, s2, s3) = pairs xs

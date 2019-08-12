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
    , mkDayUnits, mkPartDayUnits
    , tdRound
    ) where

import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit ((@?), testCase)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units ()
import Flight.Distance (QTaskDistance, TaskDistance(..), PathDistance(..), fromKms)
import Flight.Zone (Radius(..), Zone(..))
import Data.Ratio.Rounding (dpRound)
import ToLatLng (ToLatLng)

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.~=.) :: (Show a, Show b) => a -> b -> String
(.~=.) x y = show x ++ " ~= " ++ show y

toTaskDistanceR
    :: Real a
    => TaskDistance (Quantity a u)
    -> TaskDistance (Quantity Rational u)
toTaskDistanceR (TaskDistance d) =
    TaskDistance $ toRational' d

tdRound
    :: (Real a, Fractional a)
    => QTaskDistance a v -> QTaskDistance a w
tdRound (TaskDistance (MkQuantity d)) =
    TaskDistance . MkQuantity . fromRational . dpRound 3 $ toRational d

mkDayUnits
    :: (Real a)
    => ([Zone a] -> PathDistance a)
    -> TestName
    -> [Zone a]
    -> Quantity a [u| km |]
    -> [Quantity a [u| km |]]
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
        dDay = tdRound . fromKms $ toRational' dDay'
        dsDay = tdRound . fromKms . toRational' <$> dsDay'

        ppDay :: QTaskDistance Rational [u| m |]
        ppDay = tdRound . toTaskDistanceR . edgesSum $ pp pDay

        pDayInits :: [[Zone _]]
        pDayInits = drop 1 $ inits pDay

        ppDayInits :: [QTaskDistance Rational [u| m |]]
        ppDayInits = tdRound . toTaskDistanceR . edgesSum . pp <$> pDayInits

mkPartDayUnits
    :: (Real a, Fractional a)
    => ([Zone a] -> PathDistance a)
    -> TestName
    -> [Zone a]
    -> QTaskDistance a [u| m |]
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
        dKm = convert (toRational' d) :: Quantity Rational [u| km |]
        Flip r = dpRound 6 <$> Flip dKm
        tdR = TaskDistance (convert r :: Quantity Rational [u| m |])

        td'@(TaskDistance d') = edgesSum $ pp zs
        dKm' = convert (toRational' d') :: Quantity Rational [u| km |]
        Flip r' = dpRound 6 <$> Flip dKm'
        tdR' = TaskDistance (convert r' :: Quantity Rational [u| m |])

tasks :: (Ord a, Num a) => ToLatLng a -> [[Zone a]]
tasks toLL = ($ toLL) <$> [d1, d2, d3, d4, d5, d6, d7, d8]

d1
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d1 toLL =
    [ Cylinder (Radius $ MkQuantity 100) $ toLL (-33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 10000) $ toLL (-33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.85373, 147.94195)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.4397, 148.34533)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (-33.61965, 148.4099)
    ]

d2
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d2 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.12592, 147.91043))
    ]

d3
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d3 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.02107, 148.2233))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.11795, 148.5013))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-34.82197, 148.66543))
    ]

d4
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d4 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (-32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.46363, 148.989))
    ]

d5
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d5 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.56608, 148.22657))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.0164, 149.43363))
    ]

d6
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d6 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.19498, 147.76218))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-31.69323, 148.29623))
    ]

d7
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d7 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.76052, 148.64958))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-32.93585, 148.74947))
    ]

d8
    :: (Ord a, Num a)
    => ToLatLng a -> [Zone a]
d8 toLL =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (-33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (-33.75343, 147.52865))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.12908, 147.57323))
    , Cylinder (Radius $ MkQuantity 400) (toLL (-33.361, 147.9315))
    ]

type MkPart a
    = TestName
    -> [Zone a]
    -> QTaskDistance a [u| m |]
    -> TestTree

pairs :: [a] -> ([a], [a], [a])
pairs xs = (s1, s2, s3)
    where
        s1 = take 2 xs
        s2 = take 2 $ drop 1 xs
        s3 = take 2 $ drop 2 xs


p1
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> _ -> TestTree
p1 toLL mk t1 t2 t3 =
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

p2
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> _ -> TestTree
p2 toLL mk t1 t2 t3 =
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

p3
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> _ -> TestTree
p3 toLL mk t1 t2 t3 =
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

p4
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> TestTree
p4 toLL mk t1 t2 =
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

p5
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> TestTree
p5 toLL mk t1 t2 =
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

p6
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> TestTree
p6 toLL mk t1 t2 =
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

p7
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> _ -> TestTree
p7 toLL mk t1 t2 t3 =
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

p8
    :: (Ord a, Fractional a)
    => ToLatLng a -> MkPart a -> _ -> _ -> _ -> TestTree
p8 toLL mk t1 t2 t3 =
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

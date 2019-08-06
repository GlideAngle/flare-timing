{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tolerance
    ( GetTolerance
    , diff
    , showTolerance
    , describeInverseDistance
    , dblDirectChecks
    , ratDirectChecks
    , dblInverseChecks
    , ratInverseChecks
    ) where

import Prelude hiding (span)
import Data.List (zipWith5)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure ((-:), u, convert, fromRational', toRational', abs')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (LatLng, AzimuthFwd, AzimuthRev, fromDMS)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Zone (toRationalLatLng)
import qualified Flight.Geodesy as D (DirectProblem(..), DirectSolution(..))
import qualified Flight.Geodesy as I (InverseProblem(..), InverseSolution(..))
import Flight.Geodesy (DProb, DSoln, IProb, ISoln)

type GetTolerance a = Quantity a [u| m |] -> Quantity a [u| km |]

showTolerance :: (Real a, Fractional a) => Quantity a [u| m |] -> String
showTolerance d
    | dmm < [u| 1000 mm |] = show dmm
    | dkm < [u| 1 km |] = show dm
    | otherwise = show (TaskDistance dm)
    where
        dm :: Quantity Double _
        dm = fromRational' . toRational' $ d

        dkm = convert dm
        dmm = convert dm

diff :: Num a => QTaskDistance a u -> QTaskDistance a u -> QTaskDistance a u
diff (TaskDistance a) (TaskDistance b) =
    TaskDistance . abs' $ a -: b

diffAz :: DMS -> DMS -> DMS
diffAz x y =
    fromQuantity $ x' -: y'
    where
        x' :: Quantity _ [u| rad |]
        x' = toQuantity x

        y' :: Quantity _ [u| rad |]
        y' = toQuantity y

describeDirect
    :: (Real a, Fractional a)
    => (DMS, DMS)
    -> DMS
    -> QTaskDistance a [u| m |]
    -> (DMS, DMS)
    -> Quantity a [u| m |]
    -> String
describeDirect x angle s yExpected tolerance =
    show x
    ++ " "
    ++ show angle
    ++ " "
    ++ show s
    ++ " = "
    ++ show yExpected
    ++ " ± "
    ++ showTolerance tolerance

describeInverseDistance
    :: (Real a, Fractional a)
    => (DMS, DMS)
    -> (DMS, DMS)
    -> QTaskDistance a [u| m |]
    -> Quantity a [u| m |]
    -> String
describeInverseDistance x y sExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " = "
    ++ show sExpected
    ++ " ± "
    ++ showTolerance tolerance

describeAzimuthFwd
    :: (DMS, DMS)
    -> (DMS, DMS)
    -> DMS
    -> DMS
    -> String
describeAzimuthFwd x y azExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " -> "
    ++ show azExpected
    ++ " ± "
    ++ show tolerance

describeAzimuthRev
    :: (DMS, DMS)
    -> (DMS, DMS)
    -> Maybe DMS
    -> DMS
    -> String
describeAzimuthRev x y azExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " <- "
    ++ show azExpected
    ++ " ± "
    ++ show tolerance

sFoundD
    :: (LatLng Double [u| rad |] -> LatLng Double [u| rad |] -> a)
    -> (DMS, DMS)
    -> (DMS, DMS)
    -> a
sFoundD span x y =
    span (fromDMS x) (fromDMS y)

azFwdFoundD
    :: (LatLng Double [u| rad |] -> LatLng Double [u| rad |] -> a)
    -> (DMS, DMS)
    -> (DMS, DMS)
    -> a
azFwdFoundD azFwd x y =
    azFwd (fromDMS x) (fromDMS y)

azRevFoundD
    :: (LatLng Double [u| rad |] -> LatLng Double [u| rad |] -> a)
    -> (DMS, DMS)
    -> (DMS, DMS)
    -> a
azRevFoundD azRev x y =
    azRev (fromDMS x) (fromDMS y)

sFoundR
    :: (LatLng Rational [u| rad |] -> LatLng Rational [u| rad |] -> a)
    -> (DMS, DMS)
    -> (DMS, DMS)
    -> a
sFoundR span x y =
    span (fromDMS' x) (fromDMS' y)
    where
        fromDMS' = toRationalLatLng . fromDMS

expectedR :: Real a => Quantity a [u| m |] -> QTaskDistance Rational [u| m |]
expectedR d = TaskDistance $ toRational' d

dblDirectChecks
    :: GetTolerance Double
    -> [SpanLatLng Double]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks getTolerance =
    zipWith3 f
    where
        f
            span
            D.DirectSolution{D.y = y}
            D.DirectProblem{D.x = x, D.α₁ = α₁, D.s = sExpected} =
            HU.testCase (describeDirect x α₁ sExpected y tolerance)
            $ diff (sFoundD span x y) sExpected
            @?<= TaskDistance tolerance
            where
                tolerance =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected

ratDirectChecks
    :: GetTolerance Rational
    -> [SpanLatLng Rational]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks getTolerance =
    zipWith3 f
    where
        f
            span
            D.DirectSolution{D.y = y}
            D.DirectProblem{D.x = x, D.α₁ = α₁, D.s = (TaskDistance d)} =
            HU.testCase (describeDirect x α₁ sExpected y tolerance)
            $ diff (sFoundR span x y) sExpected
            @?<= TaskDistance tolerance
            where
                sExpected = expectedR d
                tolerance =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected

dblInverseChecks
    :: GetTolerance Double
    -> DMS
    -> [SpanLatLng Double]
    -> [AzimuthFwd Double]
    -> [AzimuthRev Double]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks
    getTolerance
    azTolerance
    spans
    azFwds
    azRevs
    solns
    probs =
        concat $ zipWith5 f spans azFwds azRevs solns probs
    where
        f
            span
            azFwd
            azRev
            I.InverseSolution{I.s, I.α₁, I.α₂}
            I.InverseProblem{I.x, I.y} =
            [ HU.testCase (describeInverseDistance x y s tolerance)
                $ diff (sFoundD span x y) s
                @?<= TaskDistance tolerance

            , HU.testCase (describeAzimuthFwd x y α₁ azTolerance)
                $ flip diffAz α₁ <$> (fromQuantity <$> azFwdFoundD azFwd x y) 
                @?<= Just azTolerance

            , HU.testCase (describeAzimuthRev x y α₂ azTolerance)
                $ diffAz <$> (fromQuantity <$> azRevFoundD azRev x y) <*> α₂
                @?<= Just azTolerance
            ]
            where
                tolerance =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) s

ratInverseChecks
    :: GetTolerance Rational
    -> [SpanLatLng Rational]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks getTolerance =
    zipWith3 f
    where
        f
            span
            I.InverseSolution{I.s = TaskDistance d}
            I.InverseProblem{I.x, I.y} =
            HU.testCase (describeInverseDistance x y sExpected tolerance)
            $ diff (sFoundR span x y) sExpected
            @?<= TaskDistance tolerance
            where
                sExpected = expectedR d
                tolerance =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected

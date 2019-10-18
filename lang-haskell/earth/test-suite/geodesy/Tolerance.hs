{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tolerance
    ( GetTolerance
    , AzTolerance
    , diff
    , showTolerance
    , describeInverseDistance
    , dblDirectChecks
    , ratDirectChecks
    , dblInverseChecks
    , ratInverseChecks
    ) where

import Prelude hiding (span)
import Data.Bifunctor
import Data.List (zipWith5)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure ((-:), u, convert, fromRational', toRational', abs')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..), DiffDMS)
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (LatLng, AzimuthFwd, AzimuthRev, fromDMS)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Zone (toRationalLatLng)
import qualified Flight.Geodesy as D (DirectProblem(..), DirectSolution(..))
import qualified Flight.Geodesy as I (InverseProblem(..), InverseSolution(..))
import Flight.Geodesy (DProb, DSoln, IProb, ISoln)

type GetTolerance a = Quantity a [u| m |] -> Quantity a [u| km |]
type AzTolerance = DMS

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
    -> Maybe DMS
    -> DMS
    -> DMS
    -> String
describeAzimuthFwd x y azActual azExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " -> "
    ++ show azExpected
    ++ " ± "
    ++ show tolerance
    ++ " ("
    ++ (show $ normalize <$> azActual)
    ++ ")"

describeAzimuthFwd'
    :: (DMS, DMS)
    -> (DMS, DMS)
    -> Maybe DMS
    -> DMS
    -> DMS
    -> String
describeAzimuthFwd' x y azActual _azExpected _tolerance =
    show x'
    ++ " to "
    ++ show y'
    ++ " -> _ ± _ ("
    ++ (show $ normalize <$> azActual')
    ++ ")"
    where
        x' :: (Quantity _ [u| deg |], Quantity _ [u| deg |])
        x' = bimap toQuantity toQuantity x

        y' :: (Quantity _ [u| deg |], Quantity _ [u| deg |])
        y' = bimap toQuantity toQuantity y

        azActual' :: Maybe (Quantity _ [u| deg |])
        azActual' = toQuantity <$> azActual

describeAzimuthRev
    :: (DMS, DMS)
    -> (DMS, DMS)
    -> Maybe DMS
    -> Maybe DMS
    -> DMS
    -> String
describeAzimuthRev x y azActual azExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " <- "
    ++ show azExpected
    ++ " ± "
    ++ show tolerance
    ++ " ("
    ++ (show $ normalize <$> azActual)
    ++ ")"

describeAzimuthRev'
    :: (DMS, DMS)
    -> (DMS, DMS)
    -> Maybe DMS
    -> Maybe DMS
    -> DMS
    -> String
describeAzimuthRev' x y azActual _azExpected _tolerance =
    show x'
    ++ " to "
    ++ show y'
    ++ " <- _ ± _ ("
    ++ (show $ normalize <$> azActual')
    ++ ")"
    where
        x' :: (Quantity _ [u| deg |], Quantity _ [u| deg |])
        x' = bimap toQuantity toQuantity x

        y' :: (Quantity _ [u| deg |], Quantity _ [u| deg |])
        y' = bimap toQuantity toQuantity y

        azActual' :: Maybe (Quantity _ [u| deg |])
        azActual' = toQuantity <$> azActual

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
    :: DiffDMS -- ^ Difference in forward azimuth
    -> DiffDMS -- ^ Difference in reverse azimuth
    -> GetTolerance Double
    -> AzTolerance
    -> [SpanLatLng Double]
    -> [AzimuthFwd Double]
    -> [AzimuthRev Double]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks
    diffAzFwd
    diffAzRev
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
                $ diff s' s
                @?<= TaskDistance tolerance

            , HU.testCase (describeAzimuthFwd x y α₁' α₁ azTolerance)
                $ (flip diffAzFwd) α₁ <$> α₁'
                @?<= Just azTolerance

            , HU.testCase (describeAzimuthFwd' x y α₁' α₁ azTolerance)
                $ (flip diffAzFwd) α₁ <$> α₁'
                @?<= Just azTolerance

            , HU.testCase (describeAzimuthRev x y α₂' α₂ azTolerance)
                $ diffAzRev <$> α₂' <*> α₂
                @?<= Just azTolerance

            , HU.testCase (describeAzimuthRev' x y α₂' α₂ azTolerance)
                $ diffAzRev <$> α₂' <*> α₂
                @?<= Just azTolerance
            ]
            where
                tolerance = convert . getTolerance $ (\(TaskDistance q) -> q) s
                s' = sFoundD span x y
                α₁' = fromQuantity <$> azFwdFoundD azFwd x y
                α₂' = fromQuantity <$> azRevFoundD azRev x y

ratInverseChecks
    :: GetTolerance Rational
    -> AzTolerance
    -> [SpanLatLng Rational]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks getTolerance _ =
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

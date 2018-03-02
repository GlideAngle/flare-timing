{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tolerance
    ( GetTolerance
    , diff
    , showTolerance
    , describeInverse
    , dblDirectChecks
    , ratDirectChecks
    , dblInverseChecks
    , ratInverseChecks
    ) where

import Prelude hiding (span)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure ((-:), u, convert, fromRational', toRational', abs')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng (fromDMS)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (toRationalLatLng)
import qualified Flight.Earth.Geodesy as D (DirectProblem(..), DirectSolution(..))
import qualified Flight.Earth.Geodesy as I (InverseProblem(..), InverseSolution(..))
import Flight.Earth.Geodesy (DProb, DSoln, IProb, ISoln)

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

diff :: Num a => TaskDistance a -> TaskDistance a -> TaskDistance a
diff (TaskDistance a) (TaskDistance b) =
    TaskDistance . abs' $ a -: b

describeDirect
    :: (Real a, Fractional a)
    => (DMS, DMS)
    -> DMS
    -> TaskDistance a
    -> (DMS, DMS)
    -> Quantity a [u| m |]
    -> String
describeDirect x angle s yExpected tolerance =
    show x
    ++ " bearing "
    ++ show angle
    ++ " range "
    ++ show s
    ++ " = "
    ++ show yExpected
    ++ " ± "
    ++ showTolerance tolerance

describeInverse
    :: (Real a, Fractional a)
    => (DMS, DMS)
    -> (DMS, DMS)
    -> TaskDistance a
    -> Quantity a [u| m |]
    -> String
describeInverse x y sExpected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " = "
    ++ show sExpected
    ++ " ± "
    ++ showTolerance tolerance

dblDirectChecks
    :: SpanLatLng Double
    -> GetTolerance Double
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks span getTolerance =
    zipWith f
    where
        f
            D.DirectSolution{D.y = y}
            D.DirectProblem{D.x = x, D.α₁ = α₁, D.s = sExpected} =
            HU.testCase (describeDirect x α₁ sExpected y tolerance')
            $ diff (sFound x y) sExpected
            @?<= (TaskDistance tolerance')
            where
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected

        sFound x y = span (fromDMS x) (fromDMS y)

ratDirectChecks
    :: SpanLatLng Rational
    -> GetTolerance Rational
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks span getTolerance =
    zipWith f
    where
        f
            D.DirectSolution{D.y = y}
            D.DirectProblem{D.x = x, D.α₁ = α₁, D.s = (TaskDistance d)} =
            HU.testCase (describeDirect x α₁ sExpected' y tolerance')
            $ diff (sFound x y) sExpected'
            @?<= (TaskDistance tolerance')
            where
                sExpected' = expected d
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected'

        expected d = TaskDistance $ toRational' d
        sFound x y = span (fromDMS' x) (fromDMS' y)
        fromDMS' = toRationalLatLng . fromDMS

dblInverseChecks
    :: SpanLatLng Double
    -> GetTolerance Double
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks span getTolerance =
    zipWith f
    where
        f I.InverseSolution{I.s = sExpected} I.InverseProblem{I.x, I.y} =
            HU.testCase (describeInverse x y sExpected tolerance')
            $ diff (sFound x y) sExpected
            @?<= (TaskDistance tolerance')
            where
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected

        sFound x y = span (fromDMS x) (fromDMS y)

ratInverseChecks
    :: SpanLatLng Rational
    -> GetTolerance Rational
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks span getTolerance =
    zipWith f
    where
        f I.InverseSolution{I.s = TaskDistance d} I.InverseProblem{I.x, I.y} =
            HU.testCase (describeInverse x y sExpected' tolerance')
            $ diff (sFound x y) sExpected'
            @?<= (TaskDistance tolerance')
            where
                sExpected' = expected d
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) sExpected'

        expected d = TaskDistance $ toRational' d
        sFound x y = span (fromDMS' x) (fromDMS' y)
        fromDMS' = toRationalLatLng . fromDMS

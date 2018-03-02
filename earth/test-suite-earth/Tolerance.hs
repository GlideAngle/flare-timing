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
import Flight.Earth.Geodesy
    (InverseProblem(..), InverseSolution(..), IProb, ISoln)

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

describeInverse
    :: (Real a, Fractional a)
    => (DMS, DMS)
    -> (DMS, DMS)
    -> TaskDistance a
    -> Quantity a [u| m |]
    -> String
describeInverse x y expected tolerance =
    show x
    ++ " to "
    ++ show y
    ++ " = "
    ++ show expected
    ++ " ± "
    ++ showTolerance tolerance

dblInverseChecks
    :: SpanLatLng Double
    -> GetTolerance Double
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks span getTolerance =
    zipWith f
    where
        f InverseSolution{s = expected} InverseProblem{x, y} =
            HU.testCase (describeInverse x y expected tolerance')
            $ diff (found x y) expected
            @?<= (TaskDistance tolerance')
            where
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) expected

        found x y = span (fromDMS x) (fromDMS y)

ratInverseChecks
    :: SpanLatLng Rational
    -> GetTolerance Rational
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks span getTolerance =
    zipWith f
    where
        f InverseSolution{s = TaskDistance d} InverseProblem{x, y} =
            HU.testCase (describeInverse x y expected' tolerance')
            $ diff (found x y) expected'
            @?<= (TaskDistance tolerance')
            where
                expected' = expected d
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) expected'

        expected d = TaskDistance $ toRational' d
        found x y = span (fromDMS' x) (fromDMS' y)
        fromDMS' = toRationalLatLng . fromDMS

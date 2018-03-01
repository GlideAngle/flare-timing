{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tolerance
    ( GetTolerance
    , diff
    , showTolerance
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

dblInverseChecks
    :: SpanLatLng Double
    -> GetTolerance Double
    -> [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
dblInverseChecks span getTolerance =
    zipWith f
    where
        f expected (x, y) =
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show expected
                ++ " ± "
                ++ showTolerance tolerance'
                )
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
    -> [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
ratInverseChecks span getTolerance =
    zipWith f
    where
        f (TaskDistance d) (x, y) =
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show expected'
                ++ " ± "
                ++ showTolerance tolerance'
                )
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

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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Ellipsoid.Distance where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat
    (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import Tolerance (diff, showTolerance)

span :: SpanLatLng Rational
span = Rat.distanceVincenty defEps wgs84

toDistanceEqual
    :: Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceEqual expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                (show (TaskDistance expected))
                $ found @?= (TaskDistance expected)
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

toDistanceClose
    :: Quantity Rational [u| mm |]
    -> Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceClose tolerance expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                ( "within ± "
                ++ showTolerance tolerance'
                ++ " of expected "
                ++ show (TaskDistance expected)
                )
                $ diff found (TaskDistance expected)
                @?<= (TaskDistance . toRational' $ tolerance')
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

        tolerance' = convert tolerance

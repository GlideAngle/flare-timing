module EdgeToEdge (edgeToEdgeUnits) where

import Data.Ratio((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Task as FS
import Flight.Task
    ( LatLng(..)
    , Radius(..)
    , Samples(..)
    )

edgeToEdgeUnits :: TestTree
edgeToEdgeUnits = testGroup "Zone edge shortest path unit tests"
    [ circumSampleUnits
    ]

circumSampleUnits :: TestTree
circumSampleUnits = testGroup "Points just within the zone"
    [ HU.testCase "No points generated outside a 400m cylinder" $
        (filter (> 400) $ snd $ FS.circumSample (Samples 100) (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1km cylinder" $
        (filter (> 1000) $ snd $ FS.circumSample (Samples 100) (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 10km cylinder" $
        (filter (> 10000) $ snd $ FS.circumSample (Samples 100) (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 400m cylinder" $
        (filter (< 399) $ snd $ FS.circumSample (Samples 100) (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 1km cylinder" $
        (filter (< 999) $ snd $ FS.circumSample (Samples 100) (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 10km cylinder" $
        (filter (< 9999) $ snd $ FS.circumSample (Samples 100) (Radius 10000) (LatLng (1, 1)))
        @?= [] 
    ]

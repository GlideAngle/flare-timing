module EdgeToEdge (edgeToEdgeUnits) where

import Data.Ratio((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Task as FS
import Flight.Task
    ( LatLng(..)
    , Radius(..)
    , Samples(..)
    , Tolerance(..)
    )

edgeToEdgeUnits :: TestTree
edgeToEdgeUnits = testGroup "Zone edge shortest path unit tests"
    [ circumSampleUnits
    ]

circumSampleUnits :: TestTree
circumSampleUnits = testGroup "Points just within the zone"
    [ HU.testCase "No points generated outside a 40m cylinder" $
        (filter (> 40) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 400m cylinder" $
        (filter (> 400) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1km cylinder" $
        (filter (> 1000) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 10km cylinder" $
        (filter (> 10000) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 100km cylinder" $
        (filter (> 100000) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1000km cylinder" $
        (filter (> 1000000) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 1000000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 40m cylinder" $
        (filter (< 39) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 400m cylinder" $
        (filter (< 399) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 1km cylinder" $
        (filter (< 999) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 10km cylinder" $
        (filter (< 9999) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 100km cylinder" $
        (filter (< 99999) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1m from a 1000km cylinder" $
        (filter (< 999999) $ snd $
        FS.circumSample (Samples 100) (Tolerance 1) (Radius 1000000) (LatLng (1, 1)))
        @?= [] 
    ]

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

mm100 :: Tolerance
mm100 = Tolerance $ 100 % 1000

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

mm1 :: Tolerance
mm1 = Tolerance $ 1 % 1000

samples :: Samples
samples = Samples 100

circumSampleUnits :: TestTree
circumSampleUnits = testGroup "Points just within the zone"
    [ HU.testCase "No points generated outside a 40m cylinder when within 1mm" $
        (filter (> 40) $ snd $
        FS.circumSample samples mm1 (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 400m cylinder when within 1mm" $
        (filter (> 400) $ snd $
        FS.circumSample samples mm1 (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1km cylinder when within 30mm" $
        (filter (> 1000) $ snd $
        FS.circumSample samples mm30 (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 10km cylinder when within 100mm" $
        (filter (> 10000) $ snd $
        FS.circumSample samples mm100 (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 100km cylinde when within 100mm" $
        (filter (> 100000) $ snd $
        FS.circumSample samples mm100 (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1000km cylinder when within 100mm" $
        (filter (> 1000000) $ snd $
        FS.circumSample samples mm100 (Radius 1000000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1mm from a 40m cylinder" $
        (filter (< 39) $ snd $
        FS.circumSample samples mm1 (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1mm from a 400m cylinder" $
        (filter (< 399) $ snd $
        FS.circumSample samples mm1 (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 30mm from a 1km cylinder" $
        (filter (< 999) $ snd $
        FS.circumSample samples mm30 (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 10km cylinder" $
        (filter (< 9999) $ snd $
        FS.circumSample samples mm100 (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 100km cylinder" $
        (filter (< 99999) $ snd $
        FS.circumSample samples mm100 (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 1000km cylinder" $
        (filter (< 999999) $ snd $
        FS.circumSample samples mm100 (Radius 1000000) (LatLng (1, 1)))
        @?= [] 
    ]

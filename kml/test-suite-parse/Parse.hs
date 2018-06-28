{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Data.List.Split (split, dropBlanks, dropDelims, oneOf, chunksOf)
import Text.RawString.QQ (r)
import TestNewtypes ()

import qualified Flight.Kml as K (LatLngAlt(lat, lng, altGps))
import Flight.Kml
    ( LLA
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    , mkPosition
    )
import Flight.Kml.Internal
    ( parseTimeOffsets
    , parseBaroMarks
    , parseLngLatAlt
    , showLngLatAlt
    , showLatLngAlt
    , roundTripLatLngAlt
    )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseSeconds parseTimeOffsets $ SC.getPositive <$> xs

    , SC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseAltitudes parseBaroMarks $ SC.getPositive <$> xs

    , SC.testProperty "Parse lng,lat,alt triples from [ (Float, Float, Int) ]" $
        SC.changeDepth (const 4) $
        \xs -> parseTriples parseLngLatAlt $
                (\(lng :: Double, lat :: Double, alt) ->
                    let lat' = Latitude $ toRational lat
                        lng' = Longitude $ toRational lng
                        alt' = Altitude $ SC.getPositive alt
                    in mkPosition (lat', lng', alt')) <$> xs
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseSeconds parseTimeOffsets $ QC.getPositive <$> xs

    , QC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseAltitudes parseBaroMarks $ QC.getPositive <$> xs

    -- WARNING: Failing test.
    --   *** Failed! Falsifiable (after 2 tests):
    --   [(0.3909011791596698,0.3011360590812839,Positive {getPositive = 1})]
    --   Use --quickcheck-replay '1 TFGenR 00000002137B7589000000000001E848000000000000E21F000002878F6B1480 0 12 4 0' to reproduce.
    , QC.testProperty "Parse lng,lat,alt triples from [ (Float, Float, Int) ]" $
        \xs -> parseTriples parseLngLatAlt $
                (\(lng :: Double, lat :: Double, alt) ->
                    let lat' = Latitude $ toRational lat
                        lng' = Longitude $ toRational lng
                        alt' = Altitude $ QC.getPositive alt
                    in mkPosition (lat', lng', alt')) <$> xs
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ HU.testCase "Parse time (same length)" $
        length parsedTimeOffsets @?= length expectedTimeOffsets

    , HU.testCase "Parse baro (same length)" $
        length parsedBaroMarks @?= length expectedBaroMarks

    , HU.testCase "Parse coord (same length)" $
        length parsedCoords @?= length expectedCoords

    , HU.testCase "Parse time (as expected)" $
        parsedTimeOffsets @?= expectedTimeOffsets

    , HU.testCase "Parse baro (as expected)" $
        parsedBaroMarks @?= expectedBaroMarks

    , HU.testCase "Parse coord (as expected)" $
        parsedCoords @?= expectedCoords

    , HU.testCase "Show lat,lng,alt (0.0, 0.0, 1) (as expected)" $
        showLngLatAlt (Latitude 0.0, Longitude 0.0, Altitude 1)
        @?= "0.000000,0.000000,1"

    , HU.testCase "Show lat,lng,alt (0.3909011791596698, 0.3011360590812839, 1) => (0.390901,0.301136,1 as expected)" $
        showLatLngAlt (Latitude 0.3909011791596698, Longitude 0.3011360590812839, Altitude 1)
        @?= "0.390901,0.301136,1"

    , HU.testCase "Show lng,lat,alt (0.3909011791596698, 0.3011360590812839, 1) => (0.301136,0.390901,1 as expected)" $
        showLngLatAlt (Latitude 0.3909011791596698, Longitude 0.3011360590812839, Altitude 1)
        @?= "0.301136,0.390901,1"
    ]

parseAltitudes :: (String -> [Altitude]) -> [Altitude] -> Bool
parseAltitudes parser xs =
    let strings ::  [String]
        strings = show . (\(Altitude alt) -> alt) <$> xs

        parsed :: [Altitude]
        parsed = parser $ unwords strings

    in parsed == xs

parseSeconds :: (String -> [Seconds]) -> [Seconds] -> Bool
parseSeconds parser xs =
    let strings ::  [String]
        strings = show . (\(Seconds s) -> s) <$> xs

        parsed :: [Seconds]
        parsed = parser $ unwords strings

    in parsed == xs

parseTriples :: (String -> [LLA]) -> [LLA] -> Bool
parseTriples parser xs =
    let extractLatLngAlt :: LLA -> (Latitude, Longitude, Altitude)
        extractLatLngAlt x = (K.lat x, K.lng x, K.altGps x)

        ys :: [(Double, Double, Altitude)]
        ys = (roundTripLatLngAlt . extractLatLngAlt) <$> xs

        zs :: [LLA]
        zs =
            (\(lat, lng, alt) ->
                let lat' = Latitude $ toRational lat
                    lng' = Longitude $ toRational lng
                in mkPosition (lat', lng', alt)) <$> ys

        strings ::  [String]
        strings = showLngLatAlt . extractLatLngAlt <$> xs

        parsed :: [LLA]
        parsed = parser $ unwords strings

    in parsed == zs

parsedTimeOffsets :: [Seconds]
parsedTimeOffsets = parseTimeOffsets timeOffsetsToParse
        
expectedTimeOffsets :: [Seconds]
expectedTimeOffsets =
    (\x -> Seconds (read x :: Integer)) <$>
        split (dropBlanks $ dropDelims $ oneOf " \n") timeOffsetsToParse

timeOffsetsToParse :: String
timeOffsetsToParse = [r|
0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120
125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 231 236 241 246 
12519 12524 12529 12534 12539 12544 12549 12554 12559 12564 12569 12574 12579 12584 12589 12594 12599 12604 12609 12614 12619 12624 12629 12634 12639 
12644 12649 12654 12659 12664 12669 12674 12679 12684 12689
              |]

parsedBaroMarks :: [Altitude]
parsedBaroMarks = parseBaroMarks baroMarksToParse

expectedBaroMarks :: [Altitude]
expectedBaroMarks =
    (\x -> Altitude (read x :: Integer)) <$>
        split (dropBlanks $ dropDelims $ oneOf " \n") baroMarksToParse

baroMarksToParse :: String
baroMarksToParse = [r|
221 221 221 221 221 221 221 221 221 221 221 221 221 221 221 222 222 222 222 222 222 222 222 222 222 
222 222 221 225 232 246 262 268 274 279 290 305 321 340 356 372 389 401 409 418 422 422 430 428 442 
399 397 393 384 376 368 360 350 343 340 330 319 313 313 311 311 311 311 311 311 311 312 312 312 312 
312 312 312 312 312 312 312 312 312 312
              |]

parsedCoords :: [LLA]
parsedCoords = parseLngLatAlt coordsToParse

triple :: [String] -> [LLA]
triple xs =
    case xs of
        [lng, lat, alt] ->
            let lat' = toRational (read lat :: Double)
                lng' = toRational (read lng :: Double)
                alt' = read alt :: Integer
             in [ mkPosition (Latitude lat', Longitude lng', Altitude alt') ]
        _ -> []

expectedCoords :: [LLA]
expectedCoords = 
    concatMap triple $ chunksOf 3 (split (dropBlanks $ dropDelims $ oneOf " ,\n") coordsToParse)

coordsToParse :: String
coordsToParse = [r|
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277
              |]

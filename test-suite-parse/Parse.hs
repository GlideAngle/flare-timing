{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Waypoint
    ( parseTimeOffsets
    , parseBaroMarks
    , parseCoords
    , showCoords
    , formatFloat
    )

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Data.List.Split (split, dropBlanks, dropDelims, oneOf, chunksOf)
import Text.RawString.QQ (r)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseInts parseTimeOffsets $ SC.getPositive <$> xs

    , SC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseInts parseBaroMarks $ SC.getPositive <$> xs

    -- WARNING: Failing test.
    --    there exists [(0.0,0.0,1)] such that
    --      condition is false
    , SC.testProperty "Parse lat,lng,alt triples from [ (Float, Float, Int) ]" $
        \xs -> parseTriples parseCoords $ (\(lat, lng, alt) -> (lat, lng, SC.getPositive alt)) <$> xs
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseInts parseTimeOffsets $ QC.getPositive <$> xs

    , QC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseInts parseBaroMarks $ QC.getPositive <$> xs

    , QC.testProperty "Parse lat,lng,alt triples from [ (Float, Float, Int) ]" $
        \xs -> parseTriples parseCoords $ (\(lat, lng, alt) -> (lat, lng, QC.getPositive alt)) <$> xs
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
    ]

parseInts :: (String -> [ String ]) -> [ Int ] -> Bool
parseInts parser xs =
    let strings ::  [ String ]
        strings = show <$> xs

        parsed :: [ String ]
        parsed = parser $ unwords strings

    in parsed == strings

parseTriples :: (String -> [ String ]) -> [ (Double, Double, Integer) ] -> Bool
parseTriples parser xs =
    let strings ::  [ String ]
        strings = showCoords <$> xs

        parsed :: [ String ]
        parsed = parser $ unwords strings

    in parsed == strings

parsedTimeOffsets :: [ String ]
parsedTimeOffsets = parseTimeOffsets timeOffsetsToParse
        
expectedTimeOffsets :: [ String ]
expectedTimeOffsets = split (dropBlanks $ dropDelims $ oneOf " \n") timeOffsetsToParse

timeOffsetsToParse :: String
timeOffsetsToParse = [r|
0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120
125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 231 236 241 246 
12519 12524 12529 12534 12539 12544 12549 12554 12559 12564 12569 12574 12579 12584 12589 12594 12599 12604 12609 12614 12619 12624 12629 12634 12639 
12644 12649 12654 12659 12664 12669 12674 12679 12684 12689
              |]

parsedBaroMarks :: [ String ]
parsedBaroMarks = parseBaroMarks baroMarksToParse

expectedBaroMarks :: [ String ]
expectedBaroMarks = split (dropBlanks $ dropDelims $ oneOf " \n") baroMarksToParse

baroMarksToParse :: String
baroMarksToParse = [r|
221 221 221 221 221 221 221 221 221 221 221 221 221 221 221 222 222 222 222 222 222 222 222 222 222 
222 222 221 225 232 246 262 268 274 279 290 305 321 340 356 372 389 401 409 418 422 422 430 428 442 
399 397 393 384 376 368 360 350 343 340 330 319 313 313 311 311 311 311 311 311 311 312 312 312 312 
312 312 312 312 312 312 312 312 312 312
              |]

parsedCoords :: [ String ]
parsedCoords = parseCoords coordsToParse

triple :: [ String ] -> String
triple xs =
    case xs of
        [a, b, c] -> mconcat [ formatFloat a
                             , ","
                             , formatFloat b
                             , ","
                             , c
                             ]
        _ -> concat xs

expectedCoords :: [ String ]
expectedCoords = 
    triple <$> chunksOf 3 (split (dropBlanks $ dropDelims $ oneOf " ,\n") coordsToParse)

coordsToParse :: String
coordsToParse = [r|
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277
              |]

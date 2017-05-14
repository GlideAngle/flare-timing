{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Waypoint (parseTime, parseBaro, parseCoord)

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Data.List.Split (split, splitOn, dropBlanks, dropDelims, oneOf, chunksOf)
import Text.RawString.QQ (r)
import Numeric (showFFloat)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseInts parseTime $ SC.getPositive <$> xs

    , SC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseInts parseBaro $ SC.getPositive <$> xs

    -- WARNING: Failing test.
    --    there exists [(0.0,0.0,1)] such that
    --      condition is false
    , SC.testProperty "Parse lat,lng,alt triples from [ (Float, Float, Int) ]" $
        \xs -> parseTriples parseCoord $ (\(lat, lng, alt) -> (lat, lng, SC.getPositive alt)) <$> xs
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Parse positive time offset from [ Int ]" $
        \xs -> parseInts parseTime $ QC.getPositive <$> xs

    , QC.testProperty "Parse barometric pressure from [ Int ]" $
        \xs -> parseInts parseBaro $ QC.getPositive <$> xs

    -- WARNING: Failing test.
    --    *** Failed! Falsifiable (after 2 tests and 1 shrink):
    --    [(0.0,0.5750920914065719,Positive {getPositive = 1})]
    --    Use --quickcheck-replay '1 TFGenR 0000007D32389CDB0000000000989680000000000000E21F000002ED6C734B80 0 12 4 0' to reproduce.

    , QC.testProperty "Parse lat,lng,alt triples from [ (Float, Float, Int) ]" $
        \xs -> parseTriples parseCoord $ (\(lat, lng, alt) -> (lat, lng, QC.getPositive alt)) <$> xs
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ HU.testCase "Parse time (same length)" $
        length parsedTime @?= length expectedTimeStr

    , HU.testCase "Parse baro (same length)" $
        length parsedBaro @?= length expectedBaroStr

    , HU.testCase "Parse coord (same length)" $
        length parsedCoord @?= length expectedCoordStr

    , HU.testCase "Parse time (as expected)" $
        parsedTime @?= expectedTimeStr

    , HU.testCase "Parse baro (as expected)" $
        parsedBaro @?= expectedBaroStr

    , HU.testCase "Parse coord (as expected)" $
        parsedCoord @?= expectedCoordStr
    ]

parseInts :: (String -> [ String ]) -> [ Int ] -> Bool
parseInts parser xs =
    let strings ::  [ String ]
        strings = show <$> xs

        parsed :: [ String ]
        parsed = parser $ unwords strings

    in parsed == strings

parseTriples :: (String -> [ String ]) -> [ (Double, Double, Int) ] -> Bool
parseTriples parser xs =
    let strings ::  [ String ]
        strings = (\(lat, lng, alt) ->
                      concat [ formatFloat $ show lat
                             , ","
                             , formatFloat $ show lng
                             , ","
                             , show alt
                             ]) <$> xs

        parsed :: [ String ]
        parsed = parser $ unwords strings

    in parsed == strings

parsedTime :: [ String ]
parsedTime = parseTime timeToParse
        
expectedTimeStr :: [ String ]
expectedTimeStr = split (dropBlanks $ dropDelims $ oneOf " \n") timeToParse

timeToParse :: String
timeToParse = [r|
0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120
125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 231 236 241 246 
12519 12524 12529 12534 12539 12544 12549 12554 12559 12564 12569 12574 12579 12584 12589 12594 12599 12604 12609 12614 12619 12624 12629 12634 12639 
12644 12649 12654 12659 12664 12669 12674 12679 12684 12689
              |]

parsedBaro :: [ String ]
parsedBaro = parseBaro baroToParse

expectedBaroStr :: [ String ]
expectedBaroStr = split (dropBlanks $ dropDelims $ oneOf " \n") baroToParse

baroToParse :: String
baroToParse = [r|
221 221 221 221 221 221 221 221 221 221 221 221 221 221 221 222 222 222 222 222 222 222 222 222 222 
222 222 221 225 232 246 262 268 274 279 290 305 321 340 356 372 389 401 409 418 422 422 430 428 442 
399 397 393 384 376 368 360 350 343 340 330 319 313 313 311 311 311 311 311 311 311 312 312 312 312 
312 312 312 312 312 312 312 312 312 312
              |]

parsedCoord :: [ String ]
parsedCoord = parseCoord coordToParse

rtrimZero :: String -> String
rtrimZero =
     reverse . dropWhile (== '0') . reverse

formatFloat :: String -> String
formatFloat s =
    case splits of
         [ a, b ] -> a ++ "." ++ rtrimZero b
         _ -> s
    where
        s' = showFFloat (Just 6) (read s :: Double) ""
        splits = splitOn "." s'

triple :: [ String ] -> String
triple xs =
    case xs of
        [a, b, c] ->
            concat [ "("
                   , formatFloat a
                   , ","
                   , formatFloat b
                   , ","
                   , c
                   , ")"
                   ]

        _ -> concat xs

expectedCoordStr :: [ String ]
expectedCoordStr = 
    triple <$> chunksOf 3 (split (dropBlanks $ dropDelims $ oneOf " ,\n") coordToParse)

coordToParse :: String
coordToParse = [r|
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277
              |]

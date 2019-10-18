module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-XLambdaCase"
    , "-isrc"
    , "library/Flight/Zone/SpeedSection.hs"
    ]

main :: IO ()
main = doctest arguments

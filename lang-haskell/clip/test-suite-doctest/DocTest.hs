module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Track/Range.hs"
    , "-XLambdaCase"
    , "-package split"
    ]

main :: IO ()
main = doctest arguments

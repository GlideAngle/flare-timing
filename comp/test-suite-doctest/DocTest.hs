module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Track/Range.hs"
    , "library/Flight/Track/Place.hs"
    ]

main :: IO ()
main = doctest arguments

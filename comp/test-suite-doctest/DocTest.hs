module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Track/Range.hs"
    ]

main :: IO ()
main = doctest arguments

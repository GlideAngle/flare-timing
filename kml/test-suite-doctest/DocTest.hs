module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Kml.hs"
    , "library/Flight/Types.hs"
    ]

main :: IO ()
main = doctest arguments

module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Data/Ratio/Rounding.hs"
    ]

main :: IO ()
main = doctest arguments

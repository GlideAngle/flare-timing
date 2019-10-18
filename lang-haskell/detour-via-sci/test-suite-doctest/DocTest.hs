module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Data/Via/Scientific.hs"
    ]

main :: IO ()
main = doctest arguments

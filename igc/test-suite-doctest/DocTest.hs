module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Igc.hs"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    ]

main :: IO ()
main = doctest arguments

module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/TrackLog.hs"
    , "-XScopedTypeVariables"
    , "-XTupleSections"
    , "-XParallelListComp"
    , "-XNamedFieldPuns"
    ]

main :: IO ()
main = doctest arguments

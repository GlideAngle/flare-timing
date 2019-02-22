module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Fsdb/Internal/Parse.hs"
    , "library/Flight/Fsdb/Internal/XmlPickle.hs"
    , "-XFlexibleContexts"
    , "-XPartialTypeSignatures"
    , "-XGADTs"
    ]

main :: IO ()
main = doctest arguments

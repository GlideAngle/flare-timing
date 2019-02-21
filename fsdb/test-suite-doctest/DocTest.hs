module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Fsdb/Internal/XmlPickle.hs"
    , "-XFlexibleContexts"
    , "-XPartialTypeSignatures"
    ]

main :: IO ()
main = doctest arguments

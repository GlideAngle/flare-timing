module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-XNamedFieldPuns"
    , "-XFlexibleContexts"
    , "-XMonoLocalBinds"
    , "-XMultiWayIf"
    , "-XParallelListComp"
    , "-isrc"
    , "library/Flight/Mask/Group.hs"
    ]

main :: IO ()
main = doctest arguments

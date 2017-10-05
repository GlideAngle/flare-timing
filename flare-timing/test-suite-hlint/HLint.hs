module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "test/fsdb-parser"
    , "test/igc-parser"
    , "test/kml-parser"
    , "yaml/app-serve"
    , "yaml/comp-xml-to-yaml"
    , "yaml/track-intersect-zone"
    , "test-suite-hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

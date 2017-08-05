module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "fsdb/app-serve"
    , "fsdb/app-cmd"
    , "igc/app-cmd"
    , "kml/app-cmd"
    , "yaml/app-cmd"
    , "build/app-cmd"
    , "test-suite-hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

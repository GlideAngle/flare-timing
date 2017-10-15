module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "test-apps/fsdb-parser"
    , "test-apps/igc-parser"
    , "test-apps/kml-parser"
    , "prod-apps/app-serve"
    , "prod-apps/extract-task"
    , "prod-apps/task-length"
    , "prod-apps/cross-zone"
    , "prod-apps/mask-track"
    , "test-suite-hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

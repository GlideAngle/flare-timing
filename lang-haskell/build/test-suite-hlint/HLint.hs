module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "app-cmd"
    , "test-suite-hlint"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "library"
    , "test-suite-hlint"
    , "test-suite-flat"
    , "test-suite-sphere"
    , "test-suite-ellipsoid"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

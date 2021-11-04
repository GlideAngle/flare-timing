module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "library"
    , "test-suite-hlint"
    , "test-suite-geodesy"
    , "test-suite-forbes"
    , "test-suite-forbes-r"
    , "test-suite-greda"
    , "test-suite-meridian"
    , "test-suite-meridian-r"
    , "test-suite-published"
    , "test-suite-published-r"
    , "test-suite-zones"
    , "test-suite-cylinder"
    , "test-suite-cylinder-r"
    -- WARNING: HLint turns off QuasiQuotes even if turned on in
    -- default-extensions in the cabal file, #55.
    -- SEE: https://github.com/ndmitchell/hlint/issues/223
    , "-XQuasiQuotes"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

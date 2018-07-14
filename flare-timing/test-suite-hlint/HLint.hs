module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "test-apps/fsdb-parser"
    , "test-apps/igc-parser"
    , "test-apps/kml-parser"

    , "prod-apps/extract-input"
    , "prod-apps/task-length"
    , "prod-apps/cross-zone"
    , "prod-apps/tag-zone"
    , "prod-apps/align-time"
    , "prod-apps/discard-further"
    , "prod-apps/mask-track"
    , "prod-apps/land-out"
    , "prod-apps/gap-point"

    , "test-suite-hlint"

    -- WARNING: HLint turns off QuasiQuotes even if turned on in
    -- default-extensions in the cabal file, #55.
    -- SEE: https://github.com/ndmitchell/hlint/issues/223
    , "-XQuasiQuotes"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "test-apps/fsdb-parser"
    , "test-apps/igc-parser"
    , "test-apps/kml-parser"

    , "prod-apps/align-time"
    , "prod-apps/cross-zone"
    , "prod-apps/discard-further"
    , "prod-apps/extract-input"
    , "prod-apps/far-out"
    , "prod-apps/fly-time"
    , "prod-apps/fs-arrival"
    , "prod-apps/fs-clean"
    , "prod-apps/fs-effort"
    , "prod-apps/fs-route"
    , "prod-apps/fs-score"
    , "prod-apps/fs-trim"
    , "prod-apps/gap-point"
    , "prod-apps/land-out"
    , "prod-apps/lead-area"
    , "prod-apps/mask-arrival"
    , "prod-apps/mask-bonus"
    , "prod-apps/mask-common"
    , "prod-apps/mask-effort"
    , "prod-apps/mask-lead"
    , "prod-apps/mask-reach"
    , "prod-apps/peg-frame"
    , "prod-apps/tag-zone"
    , "prod-apps/task-length"
    , "prod-apps/unpack-track"

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

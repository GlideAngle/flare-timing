module Snack (buildRules, testRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    , need
    )

buildRules :: Rules ()
buildRules = do
    phony "snack" $
        need [ "snack-siggy-chardust"
             ]

    phony "snack-siggy-chardust" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack build"

testRules :: Rules ()
testRules = do
    phony "snack-test" $
        need [ "snack-siggy-chardust-test-hlint"
             , "snack-siggy-chardust-test-doctest"
             , "snack-siggy-chardust-test-digits"
             ]

    phony "snack-siggy-chardust-test-hlint" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack run --package-nix=test-suite-hlint.nix"

    phony "snack-siggy-chardust-test-doctest" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack run --package-nix=test-suite-doctest.nix"

    phony "snack-siggy-chardust-test-digits" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack run --package-nix=test-suite-digits.nix"

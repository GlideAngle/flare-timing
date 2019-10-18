module Snack
    ( buildRules
    , testRules
    , rulesSiggyChardust
    ) where

import Development.Shake
    ( Rules
    , Action
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((<.>))

buildRules :: Rules ()
buildRules = do
    phony "snack" $
        need [ "snack-siggy-chardust"
             ]

    phony "snack-siggy-chardust" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack build"

testRule :: FilePath -> FilePath -> Action ()
testRule pkg testName =
    cmd
        (Cwd pkg)
        Shell [ "snack run --package-nix"
              , "test-suite-" ++ testName <.> ".nix"
              ]

testRules :: Rules ()
testRules = do
    phony "snack-test" $
        need [ "snack-test-siggy-chardust"
             ]

rulesSiggyChardust :: Rules ()
rulesSiggyChardust = do
    phony "snack-test-siggy-chardust" $
        need [ "snack-test-siggy-chardust-hlint"
             , "snack-test-siggy-chardust-doctest"
             , "snack-test-siggy-chardust-digits"
             ]

    phony "snack-test-siggy-chardust-hlint" $ testRule' "hlint"
    phony "snack-test-siggy-chardust-doctest" $ testRule' "doctest"
    phony "snack-test-siggy-chardust-digits" $ testRule' "digits"

    where
        testRule' = testRule "siggy-chardust"

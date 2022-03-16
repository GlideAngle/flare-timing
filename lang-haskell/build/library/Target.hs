module Target where

import Development.Shake (Rules)
import Doc (buildRules, cleanRules)
import Cmd (buildRules, cleanRules, testRules, lintRules, docTestRules)
import Nix (buildRules, cleanRules, shellRules, fromCabalRules)
import Snack (buildRules, testRules, rulesSiggyChardust)
import Pkg (buildRules)

allWants :: [ String ]
allWants =
    [ "stack-prod-apps"
    , "stack-test-apps"
    , "stack-test-suites"
    , "stack-doctest-suites"
    -- NOTE: The following targets build but I don't want them built by default.
    --, "docs"
    --, "cabal-prod-apps"
    --, "cabal-test-apps"
    --, "nix-build"
    -- WARNING: The following targets don't currently build.
    --, "view-www"
    ]

allRules :: Rules ()
allRules = do
    Target.cleanRules
    Target.buildRules
    Target.testRules

cleanRules :: Rules ()
cleanRules = do
    Doc.cleanRules
    Cmd.cleanRules
    Nix.cleanRules

buildRules :: Rules ()
buildRules = do
    Doc.buildRules
    Pkg.buildRules
    Cmd.buildRules
    Nix.buildRules
    Nix.shellRules
    Nix.fromCabalRules
    Snack.buildRules

testRules :: Rules ()
testRules = do
    Cmd.lintRules
    Cmd.testRules
    Cmd.docTestRules
    Snack.testRules
    Snack.rulesSiggyChardust

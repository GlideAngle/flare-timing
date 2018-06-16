module Target where

import Development.Shake (Rules)
import Doc (buildRules, cleanRules)
import Cmd (buildRules, cleanRules, testRules, lintRules)
import Web (buildRules, cleanRules)
import Nix (buildRules, nixRules, shellRules)
import Pkg (buildRules)
import Liquid (liquidRules)

allWants :: [ String ]
allWants =
    [ "stack-prod-apps"
    , "stack-test-apps"
    , "stack-test-suites"
    -- NOTE: The following targets build but I don't want them by default.
    --, "docs"
    --, "cabal-prod-apps"
    --, "cabal-test-apps"
    -- WARNING: The following targets don't currently build.
    --, "nix"
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
    Web.cleanRules

buildRules :: Rules ()
buildRules = do
    Doc.buildRules
    Pkg.buildRules
    Cmd.buildRules
    Web.buildRules
    Nix.buildRules
    Nix.nixRules
    Nix.shellRules

testRules :: Rules ()
testRules = do
    Cmd.testRules
    Cmd.lintRules
    Liquid.liquidRules

module Target where

import Development.Shake (Rules)
import Doc (buildRules, cleanRules)
import Cmd (buildRules, cleanRules, testRules, lintRules)
import Web (buildRules, cleanRules)
import Nix (buildRules, nixRules, shellRules)

allWants :: [ String ]
allWants =
    [ "docs"
    , "view-www"
    , "nix"
    , "cabal-prod-apps"
    , "stack-prod-apps"
    , "cabal-test-apps"
    , "stack-test-apps"
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
    Cmd.buildRules
    Web.buildRules
    Nix.buildRules
    Nix.nixRules
    Nix.shellRules

testRules :: Rules ()
testRules = do
    Cmd.testRules
    Cmd.lintRules

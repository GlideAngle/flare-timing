module Target where

import Development.Shake (Rules)
import Doc (buildRules, cleanRules)
import Cmd (buildRules, cleanRules, testRules, lintRules, nixRules)
import Web (buildRules, cleanRules)
import Nix (buildRules)

allWants :: [ String ]
allWants = [ "docs", "view-www", "nix", "cmd-apps" ]

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
    Cmd.nixRules
    Web.buildRules
    Nix.buildRules

testRules :: Rules ()
testRules = do
    Cmd.testRules
    Cmd.lintRules

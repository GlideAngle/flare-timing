module Target where

import Development.Shake (Rules)
import Doc (buildRules, cleanRules)
import Web (buildRules, cleanRules)
import Nix (buildRules)

allWants :: [ String ]
allWants = [ "docs", "view-www", "nix" ]
    
allRules :: Rules ()
allRules = do
    Target.cleanRules
    Target.buildRules

cleanRules :: Rules ()
cleanRules = do
    Doc.cleanRules
    Web.cleanRules

buildRules :: Rules ()
buildRules = do
    Doc.buildRules
    Web.buildRules
    Nix.buildRules

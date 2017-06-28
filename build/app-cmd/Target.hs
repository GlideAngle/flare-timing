module Target where

import Development.Shake (Rules, phony, need)
import Web (buildRules, cleanRules)

allWants :: [ String ]
allWants = [ "view" ]
    
allRules :: Rules ()
allRules = do
    Target.cleanRules
    Target.buildRules

cleanRules :: Rules ()
cleanRules = do
    Web.cleanRules

buildRules :: Rules ()
buildRules = do
    Web.buildRules

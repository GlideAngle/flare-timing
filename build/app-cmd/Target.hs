module Target where

import Development.Shake (Rules)
import Web (buildRules, cleanRules)

allWants :: [ String ]
allWants = [ "view" ]
    
allRules :: Rules ()
allRules = do
    Target.cleanRules
    Target.buildRules

cleanRules :: Rules ()
cleanRules =
    Web.cleanRules

buildRules :: Rules ()
buildRules =
    Web.buildRules

module Driver where

import Development.Shake (shakeArgs, shakeOptions, want)
import Target (allWants, allRules)

main :: IO ()
main = shakeArgs shakeOptions $ do

    want allWants
    allRules

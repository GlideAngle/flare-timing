module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []

import Test.Tasty (TestTree, testGroup, defaultMain)

import Sphere.Greda (units)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Earth tests" [units]

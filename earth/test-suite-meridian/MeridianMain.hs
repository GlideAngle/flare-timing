import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Sphere.Meridian as S
import qualified Ellipsoid.Meridian as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests"
        [ E.units
        , S.units
        ]

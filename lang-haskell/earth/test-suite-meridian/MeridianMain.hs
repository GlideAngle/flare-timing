import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Meridian.Sphere as S
import qualified Meridian.Ellipsoid.Vincenty as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests (with doubles)"
        [ testGroup "Haversines Math" [S.units]
        , testGroup "Vincenty Math" [V.units]
        ]

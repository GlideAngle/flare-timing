import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Meridian.Sphere as S
import qualified Meridian.Ellipsoid.Vincenty as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests (with rationals)"
        [ testGroup "Haversines Math" [S.unitsR]
        , testGroup "Vincenty Math" [V.unitsR]
        ]

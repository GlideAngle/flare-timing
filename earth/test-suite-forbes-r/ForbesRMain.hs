import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Forbes as F
import qualified Sphere.Forbes as S
import qualified Ellipsoid.AndoyerLambert.Forbes as AL
import qualified Ellipsoid.ForsytheAndoyerLambert.Forbes as FAL
import qualified Ellipsoid.Vincenty.Forbes as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests (with rationals)"
        [ testGroup "Pythagorus Math" [F.unitsR]
        , testGroup "Haversines Math" [S.unitsR]
        , testGroup "Forsythe Math" [FAL.unitsR]
        , testGroup "Andoyer Math" [AL.unitsR]
        , testGroup "Vincenty Math" [V.unitsR]
        ]

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
    testGroup "Earth tests"
        [ testGroup "Pythagorus Math" [F.units]
        , testGroup "Haversines Math" [S.units]
        , testGroup "Forsythe Math" [FAL.units]
        , testGroup "Andoyer Math" [AL.units]
        , testGroup "Vincenty Math" [V.units]
        ]

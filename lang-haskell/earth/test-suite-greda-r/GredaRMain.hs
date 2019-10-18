import Test.Tasty (TestTree, testGroup, defaultMain)

import Flat.Greda as F
import Sphere.Greda as S
import qualified Ellipsoid.Vincenty.Greda as V
import qualified Ellipsoid.AndoyerLambert.Greda as AL
import qualified Ellipsoid.ForsytheAndoyerLambert.Greda as FAL
import qualified Ellipsoid.FsAndoyer.Greda as FSA

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests (with rationals)"
        [ testGroup "Pythagorus Math" [F.unitsR]
        , testGroup "Haversines Math" [S.unitsR]
        , testGroup "Vincenty Math" [V.unitsR]
        , testGroup "Andoyer Math" [AL.unitsR]
        , testGroup "Forsythe Math" [FAL.unitsR]
        , testGroup "FS Ellipsoid Math" [FSA.unitsR]
        ]

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
    testGroup "Earth tests (with doubles)"
        [ testGroup "Pythagorus Math" [F.units]
        , testGroup "Haversines Math" [S.units]
        , testGroup "Vincenty Math" [V.units]
        , testGroup "Andoyer Math" [AL.units]
        , testGroup "Forsythe Math" [FAL.units]
        , testGroup "FS Ellipsoid Math" [FSA.units]
        ]

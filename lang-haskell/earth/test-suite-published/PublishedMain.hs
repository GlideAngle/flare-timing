import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Published.Flat as F
import qualified Published.Sphere as S
import qualified Published.Ellipsoid.Vincenty as V
import qualified Published.Ellipsoid.FsAndoyer as FS
import qualified Published.Ellipsoid.AndoyerLambert as AL
import qualified Published.Ellipsoid.ForsytheAndoyerLambert as FAL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests with published data"
        [ testGroup "Pythagorus Math" [F.units]
        , testGroup "Haversines Math" [S.units]
        , testGroup "Vincenty Math" [V.units]
        , testGroup "FsAndoyer Math" [FS.units]
        , testGroup "Andoyer Math" [AL.units]
        , testGroup "Forsythe Math" [FAL.units]
        ]

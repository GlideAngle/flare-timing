module PublishedMain where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Published as F
import qualified Sphere.Published as S
import qualified Ellipsoid.Vincenty.Published as V
import qualified Ellipsoid.AndoyerLambert.Published as AL
import qualified Ellipsoid.ForsytheAndoyerLambert.Published as FAL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests"
        [ F.units
        , S.units
        , V.units
        , AL.units
        , FAL.units
        ]

module Test.Validity.AesonSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Aeson (encode, decode)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Data.UnitsOfMeasure (u)
import Flight.Zone.Radius (Radius(..))

spec :: Spec
spec = do
    describe "ToJSON" $ do
        it "encodes a radius" $ do
            encode (Radius [u| 11.22 m |])
            `shouldBe`
            T.encodeUtf8 "\"11.2 m\""

    describe "FromJSON" $ do
        it "decodes a radius" $ do
            decode "\"11.2 m\""
            `shouldBe`
            (Just $ Radius [u| 11.2 m |])

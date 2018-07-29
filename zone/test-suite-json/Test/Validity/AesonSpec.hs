module Test.Validity.AesonSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Aeson (encode, decode)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Flight.Zone.Bearing (Bearing(..))
import Flight.Zone.Incline (Incline(..))
import Flight.Zone.Radius (QRadius, Radius(..))

spec :: Spec
spec = do
    describe "ToJSON" $ do
        it "encodes a bearing"
            $ encode bearing `shouldBe` T.encodeUtf8 "\"1.57 rad\""

        it "encodes a incline"
            $ encode incline `shouldBe` T.encodeUtf8 "\"90.0 deg\""

        it "encodes a radius"
            $ encode radius `shouldBe` T.encodeUtf8 "\"11.2 m\""

    describe "FromJSON" $ do
        it "decodes a bearing"
            $ decode "\"1.57 rad\"" `shouldBe` (Just bearing')

        it "decodes an incline"
            $ decode "\"90.0 deg\"" `shouldBe` (Just incline)

        it "decodes a radius"
            $ decode "\"11.2 m\"" `shouldBe` (Just radius')

    where
        deg90 :: Quantity Double [u| rad |]
        deg90 = convert [u| 90 deg |]

        bearing = Bearing deg90
        bearing' = Bearing [u| 1.57 rad |]

        incline = Incline deg90

        radius :: QRadius Double [u| m |]
        radius = Radius [u| 11.22 m |]

        radius' :: QRadius Double [u| m |]
        radius' = Radius [u| 11.2 m |]

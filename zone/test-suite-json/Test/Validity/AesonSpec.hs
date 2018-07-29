{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Validity.AesonSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Aeson (encode, decode)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone.AltTime (QAltTime, AltTime(..))
import Flight.Zone.Bearing (QBearing, Bearing(..))
import Flight.Zone.Incline (QIncline, Incline(..))
import Flight.Zone.Radius (QRadius, Radius(..))

spec :: Spec
spec = do
    describe "ToJSON" $ do
        it ("encodes a alt time of " ++ show altTime)
            $ encode altTime `shouldBe` T.encodeUtf8 "\"3.5 s / m\""

        it ("encodes a bearing of " ++ show bearing)
            $ encode bearing `shouldBe` T.encodeUtf8 "\"1.57 rad\""

        it ("encodes a incline of " ++ show deg90 ++ "°")
            $ encode (Incline . convert $ deg90)
            `shouldBe` T.encodeUtf8 str90

        it ("encodes a incline of " ++ show deg11 ++ "°")
            $ encode (Incline . convert $ deg11)
            `shouldBe` T.encodeUtf8 str11_3dp

        it ("encodes a radius of " ++ show radius)
            $ encode radius `shouldBe` T.encodeUtf8 "\"11.2 m\""

    describe "FromJSON" $ do
        it ("decodes an altitude time of 3.5 s / m as " ++ show altTime)
            $ decode "\"3.5 s / m\"" `shouldBe` (Just altTime)

        it ("decodes a bearing of 1.57 rad as " ++ show bearing')
            $ decode "\"1.57 rad\"" `shouldBe` (Just bearing')

        it ("decodes an incline of 90° as " ++ show incline)
            $ decode "\"90.0 deg\"" `shouldBe` (Just incline)

        it ("decodes an incline of 11.223° as " ++ show incline')
            $ decode "\"11.223 deg\"" `shouldBe` (Just incline')

        it ("decodes a radius of 11.2 m as " ++ show radius')
            $ decode "\"11.2 m\"" `shouldBe` (Just radius')

    where
        deg90 :: Quantity Double [u| deg |]
        deg90 = [u| 90 deg |]

        deg11 :: Quantity Double [u| deg |]
        deg11 = [u| 11.2233445566778899 deg |]

        deg11_3dp :: Quantity Double [u| rad |]
        deg11_3dp = convert [u| 11.223 deg |]

        str90 = "\"90.0 deg\""
        str11_3dp = "\"11.223 deg\""

        altTime :: QAltTime Double [u| s / m |]
        altTime = AltTime [u| 3.5 s / m |]

        bearing :: QBearing Double [u| rad |]
        bearing = Bearing . convert $ deg90

        bearing' :: QBearing Double [u| rad |]
        bearing' = Bearing [u| 1.57 rad |]

        incline :: QIncline Double [u| rad |]
        incline = Incline . convert $ deg90

        incline' :: QIncline Double [u| rad |]
        incline' = Incline deg11_3dp

        radius :: QRadius Double [u| m |]
        radius = Radius [u| 11.22 m |]

        radius' :: QRadius Double [u| m |]
        radius' = Radius [u| 11.2 m |]

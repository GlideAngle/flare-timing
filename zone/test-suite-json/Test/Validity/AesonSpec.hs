{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Validity.AesonSpec where

import Data.ByteString (ByteString)
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Aeson (ToJSON, encode, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Yaml.Pretty as Y
import Data.String.Here (hereLit, hereFile)

import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..))
import Flight.Zone.AltTime (QAltTime, AltTime(..))
import Flight.Zone.Bearing (QBearing, Bearing(..))
import Flight.Zone.Incline (QIncline, Incline(..))
import Flight.Zone.Radius (QRadius, Radius(..))
import Flight.Zone.ZoneKind hiding (radius) 

yenc :: ToJSON a => a -> ByteString
yenc = Y.encodePretty Y.defConfig

spec :: Spec
spec = do
    describe "To YAML" $ do
        it ("encodes an alt time of " ++ show altTime)
            $ yenc altTime `shouldBe` [hereLit|3.5 s / m
|]

    describe "To JSON" $ do
        it ("encodes an alt time of " ++ show altTime)
            $ encode altTime `shouldBe` [hereLit|"3.5 s / m"|]

        it ("encodes a bearing of " ++ show bearing)
            $ encode bearing `shouldBe` [hereLit|"1.57 rad"|]

        it ("encodes a incline of " ++ show deg90 ++ "°")
            $ encode (Incline . convert $ deg90)
            `shouldBe` str90

        it ("encodes a incline of " ++ show deg11 ++ "°")
            $ encode (Incline . convert $ deg11)
            `shouldBe` str11_3dp

        it ("encodes a radius of " ++ show radius)
            $ encode radius `shouldBe` [hereLit|"11.2 m"|]

        it "encodes a point zone kind"
            $ encodePretty zkPt
            `shouldBe`
            [hereFile|jenc/point.json|]

        it "encodes a cylinder zone kind"
            $ encodePretty zkCyl
            `shouldBe`
            [hereLit|{
    "cylinder": {
        "radius": "11.2 m",
        "center": [
            "43.82972999 deg",
            "16.64243 deg"
        ]
    }
}|]

        it "encodes a circle zone kind"
            $ encodePretty zkCircle
            `shouldBe`
            [hereLit|{
    "circle": {
        "radius": "11.2 m",
        "center": [
            "43.82972999 deg",
            "16.64243 deg"
        ]
    }
}|]

        it "encodes an ESS is goal race"
            $ encodePretty tzEssIsGoalRace
            `shouldBe`
            [hereLit|{
    "race-ess-is-goal": {
        "circle": {
            "radius": "11.2 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race": [
        {
            "cylinder": {
                "radius": "11.2 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "prolog": []
}|]

        it "encodes an ESS is not goal race"
            $ encodePretty tzEssIsNotGoalRace
            `shouldBe`
            [hereLit|{
    "race": [
        {
            "cylinder": {
                "radius": "11.2 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "epilog": [],
    "goal": {
        "circle": {
            "radius": "11.2 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race-ess": {
        "circle": {
            "radius": "11.2 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "prolog": []
}|]

        it "encodes an ESS is not goal race with prolog and epilog"
            $ encodePretty tzEssIsNotGoalRaceProEpi
            `shouldBe`
            [hereLit|{
    "race": [
        {
            "cylinder": {
                "radius": "11.2 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "epilog": [
        {
            "cylinder": {
                "radius": "11.2 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "goal": {
        "circle": {
            "radius": "11.2 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race-ess": {
        "circle": {
            "radius": "11.2 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "prolog": [
        {
            "cylinder": {
                "radius": "11.2 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ]
}|]

    describe "From JSON" $ do
        it ("decodes an altitude time of 3.5 s / m as " ++ show altTime)
            $ decode [hereLit|"3.5 s / m"|] `shouldBe` (Just altTime)

        it ("decodes a bearing of 1.57 rad as " ++ show bearing')
            $ decode [hereLit|"1.57 rad"|] `shouldBe` (Just bearing')

        it ("decodes an incline of 90° as " ++ show incline)
            $ decode [hereLit|"90.0 deg"|] `shouldBe` (Just incline)

        it ("decodes an incline of 11.223° as " ++ show incline')
            $ decode [hereLit|"11.223 deg"|] `shouldBe` (Just incline')

        it ("decodes a radius of 11.2 m as " ++ show radius')
            $ decode [hereLit|"11.2 m"|] `shouldBe` (Just radius')

        it "decodes a point zone kind"
            $ decode
            [hereLit|{
    "point": [
        "43.82972999 deg",
        "16.64243 deg"
    ]
}|]
            `shouldBe` (Just zkPt)

        it "decodes a cylinder zone kind"
            $ decode
            [hereLit|{
    "cylinder": {
        "radius": "11.22 m",
        "center": [
            "43.82972999 deg",
            "16.64243 deg"
        ]
    }
}|]
            `shouldBe` (Just zkCyl)

        it "decodes a circle zone kind"
            $ decode 
            [hereLit|{
    "circle": {
        "radius": "11.22 m",
        "center": [
            "43.82972999 deg",
            "16.64243 deg"
        ]
    }
}|]
            `shouldBe` (Just zkCircle)

        it "decodes an ESS is goal race"
            $ decode 
            [hereLit|{
    "race-ess-is-goal": {
        "circle": {
            "radius": "11.22 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race": [
        {
            "cylinder": {
                "radius": "11.22 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "prolog": []
}|]
            `shouldBe` (Just tzEssIsGoalRace)

        it "decodes an ESS is not goal race"
            $ decode 
            [hereLit|{
    "race": [
        {
            "cylinder": {
                "radius": "11.22 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "epilog": [],
    "goal": {
        "circle": {
            "radius": "11.22 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race-ess": {
        "circle": {
            "radius": "11.22 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "prolog": []
}|]
            `shouldBe` (Just tzEssIsNotGoalRace)

        it "decodes an ESS is not goal race with prolog and epilog"
            $ decode 
            [hereLit|{
    "race": [
        {
            "cylinder": {
                "radius": "11.22 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "epilog": [
        {
            "cylinder": {
                "radius": "11.22 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ],
    "goal": {
        "circle": {
            "radius": "11.22 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "race-ess": {
        "circle": {
            "radius": "11.22 m",
            "center": [
                "43.82972999 deg",
                "16.64243 deg"
            ]
        }
    },
    "prolog": [
        {
            "cylinder": {
                "radius": "11.22 m",
                "center": [
                    "43.82972999 deg",
                    "16.64243 deg"
                ]
            }
        }
    ]
}|]
            `shouldBe` (Just tzEssIsNotGoalRaceProEpi)

    where
        deg90 :: Quantity Double [u| deg |]
        deg90 = [u| 90 deg |]

        deg11 :: Quantity Double [u| deg |]
        deg11 = [u| 11.2233445566778899 deg |]

        deg11_3dp :: Quantity Double [u| rad |]
        deg11_3dp = convert [u| 11.223 deg |]

        str90 = [hereLit|"90.0 deg"|]
        str11_3dp = [hereLit|"11.223 deg"|]

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

        zkPt = Point gs1

        zkCyl :: ZoneKind Turnpoint Double
        zkCyl = Cylinder radius gs1

        zkCircle :: ZoneKind Goal Double
        zkCircle = Circle radius gs1

        tzEssIsGoalRace = TzEssIsGoal [] [zkCyl] zkCircle
        tzEssIsNotGoalRace = TzEssIsNotGoal [] [zkCyl] zkCircle [] zkCircle
        tzEssIsNotGoalRaceProEpi = TzEssIsNotGoal [zkCyl] [zkCyl] zkCircle [zkCyl] zkCircle

gs1 :: LatLng Double [u| rad |]
gs1 = LatLng 
    ( Lat $ convert [u| 43.82972999 deg |]
    , Lng $ convert [u| 16.64243 deg |]
    )


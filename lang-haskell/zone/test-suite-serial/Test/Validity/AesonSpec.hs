{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Validity.AesonSpec where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.Yaml as Y (decodeEither')
import Data.String.Here (hereFile)

import Flight.Units ()
import Flight.Field (FieldOrdering(..))
import Flight.LatLng (QAlt, Alt(..), LatLng(..), Lat(..), Lng(..))
import Flight.Zone.AltTime (QAltTime, AltTime(..))
import Flight.Zone.Bearing (QBearing, Bearing(..))
import Flight.Zone.Incline (QIncline, Incline(..))
import Flight.Zone.Radius (QRadius, Radius(..))
import Flight.Zone.Internal.ZoneKind
    (ZoneKind(..), CourseLine, Turnpoint, EndOfSpeedSection, Goal, Race, OpenDistance)
import Flight.Zone.TaskZones (TaskZones(..))

yenc :: ToJSON a => a -> BL.ByteString
yenc = BL.fromStrict . Y.encodePretty Y.defConfig

yenc' :: (ToJSON a, FieldOrdering a) => a -> BL.ByteString
yenc' x = BL.fromStrict . Y.encodePretty cfg $ x
    where cfg = Y.setConfCompare (fieldOrder x) Y.defConfig

ydec :: FromJSON a => ByteString -> Either ByteString a
ydec =
    -- NOTE: Data.Yaml.ParseException has no instance of Eq so map the left of
    -- the either to a type that does have an Eq instance.
    --
    -- • No instance for (Eq Y.ParseException)
    --    arising from a use of ‘shouldBe’
    first (const "") . Y.decodeEither'

test_encodeYaml :: TestTree
test_encodeYaml =
    testGroup "Tasks to YAML"
        [ testGroup "Single Race Tasks"
            [ goldenVsString
                "encodes an ESS is goal race"
                "yenc/ess-is-goal-race.yaml.golden"
                (return $ yenc tzEssIsGoalRace)

            , goldenVsString
                "encodes an ESS is not goal race"
                "yenc/ess-is-not-goal-race.yaml.golden"
                (return $ yenc tzEssIsNotGoalRace)

            , goldenVsString
                "encodes an ESS is not goal race with prolog and epilog"
                "yenc/ess-is-not-goal-race-pro-epi.yaml.golden"
                (return $ yenc tzEssIsNotGoalRaceProEpi)
            ]

        , testGroup "Single Open Distance Tasks"
            [ goldenVsString
                "encodes HG open distance, no heading"
                "yenc/hg-open-heading-not.comp-input.yaml.golden"
                (return $ yenc' tzHgOpenHeadingNot)

            , goldenVsString
                "encodes HG open distance, heading"
                "yenc/hg-open-heading.comp-input.yaml.golden"
                (return $ yenc' tzHgOpenHeading)
            ]

        , testGroup "Multiple Race Tasks"
            [ goldenVsString
                "encodes HG race, goal /= line"
                "yenc/hg-line-not.comp-input.yaml.golden"
                (return $ yenc' tzHgLineNotRace)

            , goldenVsString
                "encodes HG race, goal == line"
                "yenc/hg-line.comp-input.yaml.golden"
                (return $ yenc' tzHgLineRace)

            , goldenVsString
                "encodes PG race, goal /= line, decelerator == CESS"
                "yenc/pg-line-not-decelerator-cess.comp-input.yaml.golden"
                (return $ yenc' tzPgLineNotDeceleratorCess)

            , goldenVsString
                "encodes PG race, goal == line, decelerator == CESS"
                "yenc/pg-line-decelerator-cess.comp-input.yaml.golden"
                (return $ yenc' tzPgLineDeceleratorCess)

            , goldenVsString
                "encodes PG race, goal /= line, decelerator == AATB"
                "yenc/pg-line-not-decelerator-aatb.comp-input.yaml.golden"
                (return $ yenc' tzPgLineNotDeceleratorAatb)

            , goldenVsString
                "encodes PG race, goal == line, decelerator == AATB"
                "yenc/pg-line-decelerator-aatb.comp-input.yaml.golden"
                (return $ yenc' tzPgLineDeceleratorAatb)

            , goldenVsString
                "encodes PG race, goal /= line, no decelerator"
                "yenc/pg-line-not-decelerator-none.comp-input.yaml.golden"
                (return $ yenc' tzPgLineNotDeceleratorNone)

            , goldenVsString
                "encodes PG race, goal == line, no decelerator"
                "yenc/pg-line-decelerator-none.comp-input.yaml.golden"
                (return $ yenc' tzPgLineDeceleratorNone)
            ]
        ]

spec_To_YAML :: Spec
spec_To_YAML = do
    it ("encodes an alt time of " ++ show altTime)
        $ yenc altTime
        `shouldBe`
        [hereFile|yenc/alt-time.yaml|]

    it ("encodes a bearing of " ++ show bearing)
        $ yenc bearing
        `shouldBe`
        [hereFile|yenc/bearing.yaml|]

    it ("encodes a incline of " ++ show deg90 ++ "°")
        $ yenc (Incline . convert $ deg90)
        `shouldBe`
        [hereFile|yenc/incline-90.yaml|]

    it ("encodes a incline of " ++ show deg11 ++ "°")
        $ yenc (Incline . convert $ deg11)
        `shouldBe`
        [hereFile|yenc/incline-11.yaml|]

    it ("encodes a radius of " ++ show radius)
        $ yenc radius
        `shouldBe`
        [hereFile|yenc/radius.yaml|]

    it "encodes a point zone kind"
        $ yenc zkPt
        `shouldBe`
        [hereFile|yenc/point.yaml|]

    it "encodes a cylinder zone kind"
        $ yenc zkCyl
        `shouldBe`
        [hereFile|yenc/cylinder.yaml|]

    it "encodes a circle zone kind"
        $ yenc zkCircle
        `shouldBe`
        [hereFile|yenc/circle.yaml|]

spec_From_YAML :: Spec
spec_From_YAML = do
    it ("decodes an altitude time of 3.5 s / m as " ++ show altTime)
        $ ydec
        [hereFile|ydec/alt-time.yaml|]
        `shouldBe` Right altTime

    it ("decodes a bearing of 1.57 rad as " ++ show bearing')
        $ ydec
        [hereFile|ydec/bearing.yaml|]
        `shouldBe` Right bearing'

    it ("decodes an incline of 90° as " ++ show incline)
        $ ydec
        [hereFile|ydec/incline-90.yaml|]
        `shouldBe` Right incline

    it ("decodes an incline of 11.223° as " ++ show incline')
        $ ydec
        [hereFile|ydec/incline-11.yaml|]
        `shouldBe` Right incline'

    it ("decodes a radius of 11.2 m as " ++ show radius')
        $ ydec
        [hereFile|ydec/radius.yaml|]
        `shouldBe` Right radius

    it "decodes a point zone kind"
        $ ydec
        [hereFile|ydec/point.yaml|]
        `shouldBe` Right zkPt

    it "decodes a cylinder zone kind"
        $ ydec
        [hereFile|ydec/cylinder.yaml|]
        `shouldBe` Right zkCyl

    it "decodes a circle zone kind"
        $ ydec
        [hereFile|ydec/circle.yaml|]
        `shouldBe` Right zkCircle

    it "decodes an ESS is goal race"
        $ ydec
        [hereFile|ydec/ess-is-goal-race.yaml|]
        `shouldBe` Right tzEssIsGoalRace

    it "decodes an ESS is not goal race"
        $ ydec
        [hereFile|ydec/ess-is-not-goal-race.yaml|]
        `shouldBe` Right tzEssIsNotGoalRace

    it "decodes an ESS is not goal race with prolog and epilog"
        $ ydec
        [hereFile|ydec/ess-is-not-goal-race-pro-epi.yaml|]
        `shouldBe` Right tzEssIsNotGoalRaceProEpi

spec_To_JSON :: Spec
spec_To_JSON = do
    it ("encodes an alt time of " ++ show altTime)
        $ encode altTime
        `shouldBe`
        [hereFile|jenc/alt-time.json|]

    it ("encodes a bearing of " ++ show bearing)
        $ encode bearing
        `shouldBe`
        [hereFile|jenc/bearing.json|]

    it ("encodes a incline of " ++ show deg90 ++ "°")
        $ encode (Incline . convert $ deg90)
        `shouldBe`
        [hereFile|jenc/incline-90.json|]

    it ("encodes a incline of " ++ show deg11 ++ "°")
        $ encode (Incline . convert $ deg11)
        `shouldBe`
        [hereFile|jenc/incline-11.json|]

    it ("encodes a radius of " ++ show radius)
        $ encode radius
        `shouldBe`
        [hereFile|jenc/radius.json|]

    it "encodes a point zone kind"
        $ encodePretty zkPt
        `shouldBe`
        [hereFile|jenc/point.json|]

    it "encodes a cylinder zone kind"
        $ encodePretty zkCyl
        `shouldBe`
        [hereFile|jenc/cylinder.json|]

    it "encodes a circle zone kind"
        $ encodePretty zkCircle
        `shouldBe`
        [hereFile|jenc/circle.json|]

    it "encodes an ESS is goal race"
        $ encodePretty tzEssIsGoalRace
        `shouldBe`
        [hereFile|jenc/ess-is-goal-race.json|]

    it "encodes an ESS is not goal race"
        $ encodePretty tzEssIsNotGoalRace
        `shouldBe`
        [hereFile|jenc/ess-is-not-goal-race.json|]

    it "encodes an ESS is not goal race with prolog and epilog"
        $ encodePretty tzEssIsNotGoalRaceProEpi
        `shouldBe`
        [hereFile|jenc/ess-is-not-goal-race-pro-epi.json|]

spec_From_JSON :: Spec
spec_From_JSON = do
    it ("decodes an altitude time of 3.5 s / m as " ++ show altTime)
        $ decode
        [hereFile|jdec/alt-time.json|]
        `shouldBe` Just altTime

    it ("decodes a bearing of 1.57 rad as " ++ show bearing')
        $ decode
        [hereFile|jdec/bearing.json|]
        `shouldBe` Just bearing'

    it ("decodes an incline of 90° as " ++ show incline)
        $ decode
        [hereFile|jdec/incline-90.json|]
        `shouldBe` Just incline

    it ("decodes an incline of 11.223° as " ++ show incline')
        $ decode
        [hereFile|jdec/incline-11.json|]
        `shouldBe` Just incline'

    it ("decodes a radius of 11.2 m as " ++ show radius')
        $ decode
        [hereFile|jdec/radius.json|]
        `shouldBe` Just radius'

    it "decodes a point zone kind"
        $ decode
        [hereFile|jdec/point.json|]
        `shouldBe` Just zkPt

    it "decodes a cylinder zone kind"
        $ decode
        [hereFile|jdec/cylinder.json|]
        `shouldBe` Just zkCyl

    it "decodes a circle zone kind"
        $ decode
        [hereFile|jdec/circle.json|]
        `shouldBe` Just zkCircle

    it "decodes an ESS is goal race"
        $ decode
        [hereFile|jdec/ess-is-goal-race.json|]
        `shouldBe` Just tzEssIsGoalRace

    it "decodes an ESS is not goal race"
        $ decode
        [hereFile|jdec/ess-is-not-goal-race.json|]
        `shouldBe` Just tzEssIsNotGoalRace

    it "decodes an ESS is not goal race with prolog and epilog"
        $ decode
        [hereFile|jdec/ess-is-not-goal-race-pro-epi.json|]
        `shouldBe` Just tzEssIsNotGoalRaceProEpi

deg90 :: Quantity Double [u| deg |]
deg90 = [u| 90 deg |]

deg11 :: Quantity Double [u| deg |]
deg11 = [u| 11.2233445566778899 deg |]

deg11_3dp :: Quantity Double [u| rad |]
deg11_3dp = convert [u| 11.223 deg |]

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

zkPt :: ZoneKind CourseLine Double
zkPt = Point gs1

zkCyl :: ZoneKind Turnpoint Double
zkCyl = Cylinder radius gs1

zkCircle :: ZoneKind Goal Double
zkCircle = Circle radius gs1

tzEssIsGoalRace :: TaskZones Race Double
tzEssIsGoalRace = TzEssIsGoal [] [zkCyl] zkCircle

tzEssIsNotGoalRace :: TaskZones Race Double
tzEssIsNotGoalRace = TzEssIsNotGoal [] [zkCyl] zkCircle [] zkCircle

tzEssIsNotGoalRaceProEpi :: TaskZones Race Double
tzEssIsNotGoalRaceProEpi = TzEssIsNotGoal [zkCyl] [zkCyl] zkCircle [zkCyl] zkCircle

rad400 :: QRadius Double [u| m |]
rad400  = Radius [u| 400 m |]

tzHgLineNotRace :: [TaskZones Race Double]
tzHgLineNotRace =
    [ TzEssIsNotGoal
        [Cylinder rad400 bmi023]
        [Cylinder rad400 bogan]
        (Cylinder rad400 trund :: ZoneKind EndOfSpeedSection _)
        []
        (Circle rad400 bmi023 :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 bmi023]
        [ Cylinder rad400 bogan
        , Cylinder rad400 trund
        ]
        (Circle rad400 bmi023 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 bmi023]
        [Cylinder rad400 bogan]
        (Cylinder rad400 trund :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 bogan]
        (Circle rad400 bmi023 :: ZoneKind Goal _)
    ]

tzHgLineRace :: [TaskZones Race Double]
tzHgLineRace =
    [ TzEssIsNotGoal
        [Cylinder rad400 bmi023]
        [Cylinder rad400 bogan]
        (Cylinder rad400 trund :: ZoneKind EndOfSpeedSection _)
        []
        (Line rad400 bmi023 :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 bmi023]
        [ Cylinder rad400 bogan
        , Cylinder rad400 trund
        ]
        (Line rad400 bmi023 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 bmi023]
        [Cylinder rad400 bogan]
        (Cylinder rad400 trund :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 bogan]
        (Line rad400 bmi023 :: ZoneKind Goal _)
    ]

tzHgOpenHeadingNot :: TaskZones OpenDistance Double
tzHgOpenHeadingNot =
    TzOpenDistance
        []
        [ Cylinder rad400 bmi023
        , Cylinder rad400 bogan
        , Cylinder rad400 trund
        ]
        (Star rad400 bmi023 :: ZoneKind OpenDistance _)

tzHgOpenHeading :: TaskZones OpenDistance Double
tzHgOpenHeading =
    TzOpenDistance
        []
        [ Cylinder rad400 bmi023
        , Cylinder rad400 bogan
        , Cylinder rad400 trund
        ]
        (Vector (Left target) rad400 bmi023 :: ZoneKind OpenDistance _)

inc35 :: QIncline Double [u| rad |]
inc35 = Incline . convert $ [u| 15.945 deg |]

alt340 :: QAlt Double [u| m |]
alt340 = Alt [u| 340 m |]

alt198 :: QAlt Double [u| m |]
alt198 = Alt [u| 198 m |]

tb :: QAltTime Double [u| s / m |]
tb = AltTime [u| 0.45 s / m |]

tzPgLineNotDeceleratorCess :: [TaskZones Race Double]
tzPgLineNotDeceleratorCess =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCone inc35 rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        []
        (Circle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (CutCone inc35 rad400 ardle alt198 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCone inc35 rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (Circle rad400 ardle :: ZoneKind Goal _)
    ]

tzPgLineDeceleratorCess :: [TaskZones Race Double]
tzPgLineDeceleratorCess =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCone inc35 rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        []
        (SemiCircle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (CutSemiCone inc35 rad400 ardle alt198 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCone inc35 rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (SemiCircle rad400 ardle :: ZoneKind Goal _)
    ]

tzPgLineNotDeceleratorAatb :: [TaskZones Race Double]
tzPgLineNotDeceleratorAatb =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCylinder tb rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        []
        (Circle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (CutCylinder tb rad400 ardle alt198 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCylinder tb rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (Circle rad400 ardle :: ZoneKind Goal _)
    ]

tzPgLineDeceleratorAatb :: [TaskZones Race Double]
tzPgLineDeceleratorAatb =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCylinder tb rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        []
        (SemiCircle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (CutSemiCylinder tb rad400 ardle alt198 :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (CutCylinder tb rad400 alec alt340 :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (SemiCircle rad400 ardle :: ZoneKind Goal _)
    ]

tzPgLineNotDeceleratorNone :: [TaskZones Race Double]
tzPgLineNotDeceleratorNone =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (Cylinder rad400 alec :: ZoneKind EndOfSpeedSection _)
        []
        (Circle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (Circle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (Cylinder rad400 alec :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (Circle rad400 ardle :: ZoneKind Goal _)
    ]

tzPgLineDeceleratorNone :: [TaskZones Race Double]
tzPgLineDeceleratorNone =
    [ TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (Cylinder rad400 alec :: ZoneKind EndOfSpeedSection _)
        []
        (SemiCircle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsGoal
        [Cylinder rad400 ways4]
        [ Cylinder rad400 afarm
        , Cylinder rad400 alec
        ]
        (SemiCircle rad400 ardle :: ZoneKind Goal _)

    , TzEssIsNotGoal
        [Cylinder rad400 ways4]
        [Cylinder rad400 afarm]
        (Cylinder rad400 alec :: ZoneKind EndOfSpeedSection _)
        [Cylinder rad400 afarm]
        (SemiCircle rad400 ardle :: ZoneKind Goal _)
    ]

gs1 :: LatLng Double [u| rad |]
gs1 = LatLng (Lat $ convert [u| 43.82972999 deg |], Lng $ convert [u| 16.64243 deg |])

bmi023 :: LatLng Double [u| rad |]
bmi023 = LatLng (Lat $ convert [u| - 33.3562 deg |], Lng $ convert [u| 147.9336 deg |])

bogan :: LatLng Double [u| rad |]
bogan = LatLng (Lat $ convert [u| - 33.12592 deg |], Lng $ convert [u| 147.91043 deg |])

trund :: LatLng Double [u| rad |]
trund = LatLng (Lat $ convert [u| - 32.9378 deg |], Lng $ convert [u| 147.7097 deg |])

target :: LatLng Double [u| rad |]
target = LatLng (Lat $ convert [u| 90 deg |], Lng $ convert [u| 0 deg |])

ways4 :: LatLng Double [u| rad |]
ways4 = LatLng (Lat $ convert [u| - 32.4916 deg |], Lng $ convert [u| 147.91973 deg |])

afarm :: LatLng Double [u| rad |]
afarm = LatLng (Lat $ convert [u| - 33.1903 deg |], Lng $ convert [u| 147.9857 deg |])

alec :: LatLng Double [u| rad |]
alec = LatLng (Lat $ convert [u| - 32.897 deg |], Lng $ convert [u| 148.2638 deg |])

ardle :: LatLng Double [u| rad |]
ardle = LatLng (Lat $ convert [u| - 34.4125 deg |], Lng $ convert [u| 146.8543 deg |])

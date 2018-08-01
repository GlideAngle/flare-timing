{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Zone.MkZones
    ( Discipline(..)
    , Decelerator(..)
    , CessIncline(..)
    , EssVsGoal(..)
    , GoalLine(..)
    , DeceleratorShape(..)
    , tpKindShape
    , essKindShape
    , goalKindShape
    , mkTpKind
    , mkEssKind
    , mkGoalKind
    , raceKindCyl
    , openKindCyl
    , openKindVec
    ) where

import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..)
    , defaultOptions, genericToJSON, genericParseJSON
    )
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..))
import Flight.Zone (QAltTime, QIncline, Incline(..))
import Flight.Zone.Internal.ZoneKind
    ( ZoneKind(..), OpenDistance
    , Turnpoint, EndOfSpeedSection, EssAllowedZone, GoalAllowedZone
    )
import Flight.Zone.TaskZones (ToZoneKind)

raceKindCyl :: EssAllowedZone k => ToZoneKind k
raceKindCyl r x _ = Cylinder r x

openKindCyl :: ToZoneKind OpenDistance
openKindCyl r x _ = Star r x

openKindVec :: LatLng Rational [u| deg |] -> ToZoneKind OpenDistance
openKindVec (LatLng (Lat dLat, Lng dLng)) =
    let rLat :: Quantity _ [u| rad |]
        rLat = fromRational' . convert $ dLat

        rLng :: Quantity _ [u| rad |]
        rLng = fromRational' . convert $ dLng

        y = LatLng (Lat rLat, Lng rLng)

    in \r x _ -> Vector (Left y) r x

data Discipline
    = HangGliding
    | Paragliding
    deriving (Eq, Ord, Generic)

disciplineOptions :: Options
disciplineOptions =
    defaultOptions
        { constructorTagModifier = \case
            "HangGliding" -> "hg"
            "Paragliding" -> "pg"
            x -> x
        }

instance Show Discipline where
    show HangGliding = "hg"
    show Paragliding = "pg"

instance Read Discipline where
    readsPrec _ ('h' : 'g' : s) = [(HangGliding, s)]
    readsPrec _ ('p' : 'g' : s) = [(Paragliding, s)]
    readsPrec _ _ = []

instance ToJSON Discipline where
  toJSON = genericToJSON disciplineOptions

instance FromJSON Discipline where
  parseJSON = genericParseJSON disciplineOptions

-- | How many units of distance for each unit of altitude, expressed like
-- a glide ratio, the n of an n : 1 glide.
newtype CessIncline =
    CessIncline Double
    deriving (Eq, Ord, Show)
    deriving newtype Num

data Decelerator
    = CESS CessIncline
    | AATB (QAltTime Double [u| s / m |])
    | NoDecelerator String
    deriving (Eq, Ord, Show)

data EndShape
    = EndLine
    | EndSemiCircle
    | EndCircle
    | EndCylinder
    | EndCutCylinder
    | EndCutSemiCylinder
    | EndCutCone
    | EndCutSemiCone

data EssVsGoal = EssBeforeGoal | EssAtGoal
data GoalLine = GoalLine | GoalNotLine
data DeceleratorShape = DecCyl | DecCone

goalKindShape
    :: Discipline
    -> GoalLine
    -> EssVsGoal
    -> Maybe DeceleratorShape
    -> EndShape
goalKindShape Paragliding GoalLine EssAtGoal (Just DecCone) = EndCutSemiCone
goalKindShape Paragliding _ EssAtGoal (Just DecCone) = EndCutCone
goalKindShape Paragliding GoalLine EssAtGoal (Just DecCyl) = EndCutSemiCylinder
goalKindShape Paragliding _ EssAtGoal (Just DecCyl) = EndCutCylinder
goalKindShape Paragliding GoalLine _ _ = EndSemiCircle
goalKindShape _ GoalLine _ _ = EndLine
goalKindShape _ _ _ _ = EndCircle

essKindShape
    :: Discipline
    -> GoalLine
    -> EssVsGoal
    -> Maybe DeceleratorShape
    -> EndShape
essKindShape Paragliding GoalLine EssAtGoal (Just DecCone) = EndCutSemiCone
essKindShape Paragliding _ _ (Just DecCone) = EndCutCone
essKindShape Paragliding GoalLine EssAtGoal _ = EndCutSemiCylinder
essKindShape Paragliding _ _ _ = EndCutCylinder
essKindShape _ _ _ _ = EndCylinder

tpKindShape :: Discipline -> GoalLine -> EndShape
tpKindShape _ _ = EndCylinder

mkGoalKind
    :: (EssAllowedZone k, GoalAllowedZone k)
    => EndShape
    -> Maybe Decelerator
    -> ToZoneKind k
mkGoalKind EndCutSemiCone (Just (CESS i)) =
    \r x -> \case
        (Just alt) -> CutSemiCone (mkIncline i) r x alt
        Nothing -> SemiCircle r x
mkGoalKind EndCutCone (Just (CESS i)) =
    \r x -> \case
        (Just alt) -> CutCone (mkIncline i) r x alt
        Nothing -> Circle r x
mkGoalKind EndCutSemiCylinder (Just (AATB aatb)) =
    \r x -> \case
        (Just alt) -> CutSemiCylinder aatb r x alt
        Nothing -> SemiCircle r x
mkGoalKind EndCutCylinder (Just (AATB aatb)) =
    \r x -> \case
        (Just alt) -> CutCylinder aatb r x alt
        Nothing -> Circle r x
mkGoalKind EndSemiCircle _ =
    \r x _ -> SemiCircle r x
mkGoalKind EndLine _ =
    \r x _ -> Line r x
mkGoalKind _ _ =
    \r x _ -> Circle r x

mkTpKind :: EndShape -> ToZoneKind Turnpoint
mkTpKind _ r x _ = Cylinder r x

mkEssKind
    :: EndShape
    -> Maybe Decelerator
    -> ToZoneKind EndOfSpeedSection
mkEssKind EndCutSemiCone (Just (CESS i)) =
    \r x -> \case
        (Just alt) -> CutSemiCone (mkIncline i) r x alt
        Nothing -> SemiCircle r x
mkEssKind EndCutCone (Just (CESS i)) =
    \r x -> \case
        (Just alt) -> CutCone (mkIncline i) r x alt
        Nothing -> Cylinder r x
mkEssKind EndCutSemiCylinder (Just (AATB aatb)) =
    \r x -> \case
        (Just alt) -> CutSemiCylinder aatb r x alt
        Nothing -> Cylinder r x
mkEssKind EndCutCylinder (Just (AATB aatb)) =
    \r x -> \case
        (Just alt) -> CutCylinder aatb r x alt
        Nothing -> Cylinder r x
mkEssKind _ _ =
    \r x _ -> Cylinder r x

mkIncline :: CessIncline -> QIncline Double [u| rad |]
mkIncline (CessIncline x) =
    Incline . MkQuantity $ atan2 1 x

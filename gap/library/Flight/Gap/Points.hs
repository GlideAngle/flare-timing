-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Points
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Discipline(..)
    , Penalty(..)
    , Points(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalty
    , availablePoints
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..)
    , defaultOptions, genericToJSON, genericParseJSON
    )

import Flight.Gap.Points.Distance
    (DistancePoints(..), LinearPoints(..), DifficultyPoints(..))
import Flight.Gap.Points.Time (TimePoints(..))
import Flight.Gap.Points.Leading (LeadingPoints(..))
import Flight.Gap.Points.Arrival (ArrivalPoints(..))
import Flight.Gap.Points.Task (TaskPoints(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Weighting (Weights(..))
import Flight.Gap.Weight.Distance (DistanceWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))

newtype LaunchToSssPoints = LaunchToSssPoints Rational deriving (Eq, Show)
newtype MinimumDistancePoints = MinimumDistancePoints Rational deriving (Eq, Show)

-- | Jumped the gun by this many seconds.
newtype JumpedTheGun = JumpedTheGun Rational deriving (Eq, Show)

-- | For this many seconds, loose 1 point.
newtype SecondsPerPoint = SecondsPerPoint Rational deriving (Eq, Show)

-- | A jump of this many seconds incurs the maximum penalty, the score for
-- minimum distance.
newtype JumpTheGunLimit = JumpTheGunLimit Rational deriving (Eq, Show)

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

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

data Penalty a where
    JumpedTooEarly :: MinimumDistancePoints -> Penalty Hg
    Jumped :: SecondsPerPoint -> JumpedTheGun -> Penalty Hg
    JumpedNoGoal :: SecondsPerPoint -> JumpedTheGun -> Penalty Hg
    NoGoalHg :: Penalty Hg
    Early :: LaunchToSssPoints -> Penalty Pg
    NoGoalPg :: Penalty Pg

data PointPenalty
    = PenaltyPoints Integer
    | PenaltyFraction Rational
    deriving (Show)

deriving instance Eq (Penalty a)
deriving instance Show (Penalty a)

data Points =
    Points 
        { reach :: LinearPoints
        , effort :: DifficultyPoints
        , distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type TaskPointTally = Points -> TaskPoints

zeroPoints :: Points
zeroPoints =
    Points
        { reach = LinearPoints 0
        , effort = DifficultyPoints 0
        , distance = DistancePoints 0
        , leading = LeadingPoints 0
        , time = TimePoints 0
        , arrival = ArrivalPoints 0
        }

tallyPoints :: forall a. Maybe (Penalty a) -> TaskPointTally

tallyPoints Nothing =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        TaskPoints $ linear + diff + l + t + a

tallyPoints (Just (JumpedTooEarly (MinimumDistancePoints p))) =
    const $ TaskPoints p

tallyPoints (Just (JumpedNoGoal secs jump)) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        jumpTheGun secs jump
        . TaskPoints
        $ linear + diff + l + (8 % 10) * (t + a)

tallyPoints (Just (Jumped secs jump)) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        jumpTheGun secs jump
        . TaskPoints
        $ linear + diff + l + t + a

tallyPoints (Just NoGoalHg) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        TaskPoints
        $ linear + diff + l + (8 % 10) * (t + a)

tallyPoints (Just (Early (LaunchToSssPoints d))) =
    const $ TaskPoints d

tallyPoints (Just NoGoalPg) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        } ->
        TaskPoints $ linear + diff + l

jumpTheGun :: SecondsPerPoint -> JumpedTheGun -> TaskPoints -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    TaskPoints $ max 0 (pts - jump / secs)

taskPoints :: forall a. Maybe (Penalty a) -> Points -> TaskPoints
taskPoints = tallyPoints

applyPointPenalty :: PointPenalty-> TaskPoints -> TaskPoints
applyPointPenalty (PenaltyPoints n) (TaskPoints p) =
    TaskPoints $ p - (n % 1)
applyPointPenalty (PenaltyFraction n) (TaskPoints p) =
    TaskPoints $ p - p * n

availablePoints :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePoints (TaskValidity tv) Weights{..} =
    (pts, tallyPoints Nothing pts)
    where
        DistanceWeight dw = distance
        linear = dw / 2
        diff = dw / 2
        LeadingWeight lw = leading
        ArrivalWeight aw = arrival
        TimeWeight tw = time
        k x = 1000 * tv * x

        pts =
            Points
                { reach = LinearPoints . k $ linear
                , effort = DifficultyPoints . k $ diff
                , distance = DistancePoints . k $ linear + diff
                , leading = LeadingPoints . k $ lw
                , arrival = ArrivalPoints . k $ aw
                , time = TimePoints . k $ tw
                }

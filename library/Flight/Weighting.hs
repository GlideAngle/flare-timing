module Flight.Weighting where

type GoalRatio = Rational
type DistanceWeight = Rational
type LeadingWeight = Rational
type ArrivalWeight = Rational

data HgPg = Hg | Pg deriving (Eq, Show)

distanceWeight :: GoalRatio -> DistanceWeight
distanceWeight = undefined

leadingWeight :: HgPg -> DistanceWeight -> LeadingWeight
leadingWeight = undefined

arrivalWeight :: HgPg -> DistanceWeight -> ArrivalWeight
arrivalWeight = undefined

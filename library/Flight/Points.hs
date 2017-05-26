module Flight.Points where

newtype DistancePoints = DistancePoints Rational deriving (Eq, Show)
newtype LeadingPoints = LeadingPoints Rational deriving (Eq, Show)
newtype TimePoints = TimePoints Rational deriving (Eq, Show)
newtype ArrivalPoints = ArrivalPoints Rational deriving (Eq, Show)
newtype TaskPoints = TaskPoints Rational deriving (Eq, Show)

taskPoints :: DistancePoints
              -> LeadingPoints
              -> TimePoints
              -> ArrivalPoints
              -> TaskPoints
taskPoints (DistancePoints d) (LeadingPoints l) (TimePoints t) (ArrivalPoints a) =
    TaskPoints $ d + l + t + a

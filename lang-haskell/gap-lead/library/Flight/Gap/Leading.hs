module Flight.Gap.Leading
    ( TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcPoint(..)
    , LcSeq(..)
    , LcTrack
    , LcArea
    , LcCoef
    , TaskDeadline(..)
    , LengthOfSs(..)
    , madeGoal
    , cleanTrack
    , leadingFraction
    , clampToEss
    , clampToDeadline
    , showSecs
    , isRaceLeg
    ) where

import Prelude hiding (seq)
import "newtype" Control.Newtype (Newtype(..))
import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure ((-:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces)
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Units ()
import "flight-gap-allot" Flight.Score
    (LeadingFraction(..), PowerExponent(..), powerFraction)
import Flight.Gap.Leading.Area (LeadingAreas(..), LeadingArea(..))
import Flight.Gap.Leading.Coef (LeadingCoef(..), LeadingCoefUnits)

-- | Time in seconds from the moment the first pilot crossed the start of the speed
-- section.
newtype TaskTime = TaskTime (Quantity Rational [u| s |])
    deriving (Eq, Ord)

instance Show TaskTime where
    show (TaskTime (MkQuantity t)) = showSecs t

showSecs :: Rational -> String
showSecs t =
    show i ++ "s"
    where
        d :: Double
        d = fromRational t

        i :: Int
        i = truncate d

-- | Time in seconds for the close of the task, the task deadline.
newtype TaskDeadline = TaskDeadline (Quantity Rational [u| s |])
    deriving (Eq, Ord, Show)

-- | The distance in km to the end of the speed section.
newtype DistanceToEss = DistanceToEss (Quantity Rational [u| km |])
    deriving (Eq, Ord)

instance
    (q ~ Quantity Rational [u| km |])
    => Newtype DistanceToEss q where
    pack = DistanceToEss
    unpack (DistanceToEss a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''DistanceToEss

instance Show DistanceToEss where
    show x = show $ ViaQ x

-- | The length of the speed section in km.
newtype LengthOfSs = LengthOfSs (Quantity Rational [u| km |])
    deriving (Eq, Ord, Show)

data Leg
    = PrologLeg Int
    | RaceLeg Int
    | EpilogLeg Int
    | CrossingLeg Int Int
    | LandoutLeg Int
    deriving (Eq, Ord, Show)

data LcPoint =
    LcPoint
        { leg :: Leg
        , mark :: TaskTime
        , togo :: DistanceToEss
        }
    deriving (Eq, Ord, Show)

newtype LcSeq a = LcSeq{seq :: [a]} deriving (Eq, Ord, Show)

-- | A pilot's track where points are task time paired with distance to the end
-- of the speed section.
type LcTrack = LcSeq LcPoint

type LcArea u = LeadingAreas (LcSeq (LeadingArea u)) (LeadingArea u)
type LcCoef = LcSeq (LeadingCoef LeadingCoefUnits)

madeGoal :: LcTrack -> Bool
madeGoal LcSeq{seq = xs} =
    any (\LcPoint{togo = DistanceToEss d} -> d <= [u| 0 km |]) xs

-- | Removes points where the task time < 0.
positiveTime :: LcTrack -> LcTrack
positiveTime x@LcSeq{seq = xs} =
    x{seq = filter (\LcPoint{mark = TaskTime t} -> t > [u| 0 s |]) xs}

-- | Removes points where the distance to ESS increases.
towardsGoal :: LcTrack -> LcTrack
towardsGoal x@LcSeq{seq = xs}
    | null xs = x
    | otherwise =
        x{seq = catMaybes $ zipWith f (zeroth : xs) xs}
        where
            LcPoint{mark = TaskTime tN, togo = dist} = head xs
            zeroth =
                LcPoint
                    { leg = PrologLeg 0
                    , mark = TaskTime $ tN -: [u| 1 s |]
                    , togo = dist
                    }

            f LcPoint{togo = dM} n@LcPoint{togo = dN} =
                if dM < dN then Nothing else Just n

-- | Removes initial points, those that are;
-- * further from ESS than the course length
-- * already past the end of the speed section.
initialOffside :: LengthOfSs -> LcTrack -> LcTrack
initialOffside (LengthOfSs len) x@LcSeq{seq = xs} =
    x{seq =
        dropWhile
            (\LcPoint{togo = DistanceToEss d} -> d > len || d < [u| 0 km |])
            xs
     }

cleanTrack :: LengthOfSs -> LcTrack -> LcTrack
cleanTrack len = towardsGoal . positiveTime . initialOffside len

isRaceLeg :: Leg -> Bool
isRaceLeg (RaceLeg _) = True
isRaceLeg _ = False

clampToDeadline :: TaskDeadline -> LcTrack -> LcTrack
clampToDeadline (TaskDeadline deadline) x@LcSeq{seq} =
    x{seq = clamp <$> seq}
    where
        clamp p@LcPoint{mark = TaskTime t} =
            p{mark = TaskTime $ min deadline t}

clampToEss :: LcTrack -> LcTrack
clampToEss x@LcSeq{seq} =
    x{seq = clamp <$> seq}
    where
        clamp p@LcPoint{togo = DistanceToEss dist} =
            p{togo = DistanceToEss $ max [u| 0 km |] dist}

leadingFraction
    :: LeadingCoef LeadingCoefUnits
    -> LeadingCoef LeadingCoefUnits
    -> LeadingFraction
leadingFraction (LeadingCoef (MkQuantity lcMin)) (LeadingCoef (MkQuantity lc)) =
    LeadingFraction . toRational $ powerFraction (PowerExponent $ 2/3) lcMin lc

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Gap.Leading
    ( TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcPoint(..)
    , LcSeq(..)
    , LcTrack
    , LcArea
    , TaskDeadline(..)
    , LengthOfSs(..)
    , madeGoal
    , cleanTrack
    , areaScaling
    , areaSteps
    , leadingCoefficient
    , leadingFractions
    , clampToEss
    , clampToDeadline
    , leadingFraction
    , showSecs
    ) where

import Prelude hiding (seq)
import Control.Newtype (Newtype(..))
import Control.Arrow (second)
import Data.Ratio ((%))
import Data.List (partition, sortBy)
import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Flight.Gap.Ratio (pattern (:%))
import Flight.Gap.Ratio.Leading
    (LeadingAreaStep(..), LeadingCoefficient(..), LeadingFraction(..), EssTime(..))

-- | Time in seconds from the moment the first pilot crossed the start of the speed
-- section.
newtype TaskTime = TaskTime Rational deriving (Eq, Ord)

instance Show TaskTime where
    show (TaskTime t) = showSecs t

showSecs :: Rational -> String
showSecs t =
    show i ++ "s"
    where
        d :: Double
        d = fromRational t

        i :: Int
        i = truncate d

-- | Time in seconds for the close of the task. 
-- section.
newtype TaskDeadline = TaskDeadline Rational deriving (Eq, Ord, Show)

-- | The distance in km to the end of the speed section.
newtype DistanceToEss = DistanceToEss Rational deriving (Eq, Ord)

instance Show DistanceToEss where
    show (DistanceToEss d) = show (fromRational d :: Double) ++ "km"

-- | The length of the speed section in km.
newtype LengthOfSs = LengthOfSs Rational deriving (Eq, Ord, Show)

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

data LcSeq a =
    LcSeq
        { seq :: [a]
        , extra :: Maybe a
        }
        deriving (Eq, Ord, Show)

-- | A pilot's track where points are task time paired with distance to the end
-- of the speed section.
type LcTrack = LcSeq LcPoint

type LcArea = LcSeq LeadingAreaStep

madeGoal :: LcTrack -> Bool
madeGoal LcSeq{seq = xs} =
    any (\LcPoint{togo = DistanceToEss d} -> d <= 0) xs

-- | Removes points where the task time < 0.
positiveTime :: LcTrack -> LcTrack
positiveTime x@LcSeq{seq = xs} =
    x{seq = filter (\LcPoint{mark = TaskTime t} -> t > 0) xs}

-- | Removes points where the distance to ESS increases.
towardsGoal :: LcTrack -> LcTrack
towardsGoal x@LcSeq{seq = xs}
    | null xs = x
    | otherwise =
        x{seq = catMaybes $ zipWith f (zero : xs) xs}
        where
            LcPoint{mark = TaskTime tN, togo = dist} = head xs
            zero =
                LcPoint
                    { leg = PrologLeg 0
                    , mark = TaskTime $ tN - (1 % 1)
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
            (\LcPoint{togo = DistanceToEss d} -> d > len || d < 0)
            xs
     }

cleanTrack :: LengthOfSs -> LcTrack -> LcTrack
cleanTrack len = towardsGoal . positiveTime . initialOffside len 

-- | Calculate the leading coefficient for a single track.
areaSteps
    :: TaskDeadline
    -> LengthOfSs
    -> LcTrack
    -> LcArea
areaSteps _ (LengthOfSs (0 :% _)) LcSeq{seq = xs} =
    LcSeq
        { seq = const (LeadingAreaStep $ 0 % 1) <$> xs
        , extra = Nothing
        }

areaSteps _ _ LcSeq{seq = []} =
    LcSeq
        { seq = []
        , extra = Nothing
        }

areaSteps
    deadline@(TaskDeadline dl)
    len
    track@LcSeq{seq = xs', extra}
    | dl <= 1 =
        LcSeq
            { seq = const (LeadingAreaStep $ 0 % 1) <$> xs'
            , extra = Nothing
            }
    | otherwise =
        LcSeq
            { seq = LeadingAreaStep . (* areaScaling len) <$> steps
            , extra = LeadingAreaStep . g <$> extra 
            }
        where
            withinDeadline :: LcTrack
            withinDeadline = clampToEss . clampToDeadline deadline $ track

            ys :: [LcPoint]
            ys = (\LcSeq{seq = xs} -> xs) withinDeadline

            ys' =
                case ys of
                    [] -> []
                    (y : _) -> y : ys

            f :: LcPoint -> LcPoint -> Rational
            f
                LcPoint{mark = TaskTime tM, togo = DistanceToEss dM}
                LcPoint{leg, mark = TaskTime tN, togo = DistanceToEss dN}
                | tM == tN = 0
                | tN < 0 = 0
                | not (isRaceLeg leg) = 0
                | otherwise = tN * (dM * dM - dN * dN)

            g :: LcPoint -> Rational
            g LcPoint{mark = TaskTime t, togo = DistanceToEss d} =
                t * d * d

            steps :: [Rational]
            steps = zipWith f ys' ys

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
            p{togo = DistanceToEss $ max 0 dist}

areaScaling :: LengthOfSs -> Rational
areaScaling (LengthOfSs len) =
    let (n :% d) = len * len in (1 % 1800) * (d % n)

-- | Calculate the leading coefficient for a single track.
leadingCoefficient
    :: TaskDeadline
    -> LengthOfSs
    -> LcTrack
    -> LeadingCoefficient
leadingCoefficient deadline len xs =
    LeadingCoefficient . toRational . sum
    $ (\(LeadingAreaStep x) -> x)
    <$> seq (areaSteps deadline len xs)

-- | Calculate the leading coefficient for all tracks.
leadingCoefficients
    :: TaskDeadline
    -> LengthOfSs
    -> [LcTrack]
    -> [LeadingCoefficient]
leadingCoefficients deadline@(TaskDeadline maxTaskTime) len tracks =
    snd <$> csSorted
    where
        cleanXs :: [LcTrack]
        cleanXs = cleanTrack len <$> tracks

        iXs :: [(Int, LcTrack)]
        iXs = zip [1 .. ] cleanXs

        (xsMadeGoal :: [(Int, LcTrack)], xsLandedOut :: [(Int, LcTrack)]) =
            partition
                (\(_, track) -> madeGoal track)
                iXs

        essTimes :: [TaskTime]
        essTimes =
            catMaybes $ safeLast <$> xsMadeGoal
            where
                safeLast (_, LcSeq{seq = xs}) =
                    if null xs then Nothing else Just (mark $ last xs)

        (TaskTime essTime) =
            if null essTimes then TaskTime maxTaskTime else maximum essTimes

        (xsEarly :: [(Int, LcTrack)], xsLate :: [(Int, LcTrack)]) =
            partition
                (\(_, LcSeq{seq = xs}) ->
                    all (\LcPoint{mark = TaskTime t} -> t < essTime) xs)
                xsLandedOut

        lc :: LcTrack -> LeadingCoefficient
        lc = leadingCoefficient deadline len

        csMadeGoal :: [(Int, LeadingCoefficient)]
        csMadeGoal = second lc <$> xsMadeGoal

        lcE :: LcTrack -> LeadingCoefficient
        lcE track@LcSeq{seq = xs} =
            if null xs then LeadingCoefficient $ 0 % 1 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b =
                    (\LcPoint{togo = DistanceToEss d} -> essTime * d * d)
                    $ last xs

        csEarly :: [(Int, LeadingCoefficient)]
        csEarly = second lcE <$> xsEarly

        lcL :: LcTrack -> LeadingCoefficient
        lcL track@LcSeq{seq = xs} =
            if null xs then LeadingCoefficient $ 0 % 1 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b =
                    (\LcPoint{ mark = TaskTime t
                             , togo = DistanceToEss d
                             } -> t * d * d)
                    $ last xs

        csLate :: [(Int, LeadingCoefficient)]
        csLate = second lcL <$> xsLate

        csMerged :: [(Int, LeadingCoefficient)]
        csMerged = mconcat [ csMadeGoal, csEarly, csLate ]

        csSorted :: [(Int, LeadingCoefficient)]
        csSorted = sortBy (\x y -> fst x `compare` fst y) csMerged

leadingDenominator :: Rational -> Double
leadingDenominator cMin = fromRational cMin ** (1 / 2)

leadingFraction :: LeadingCoefficient -> LeadingCoefficient -> LeadingFraction
leadingFraction (LeadingCoefficient 0) _ =
    LeadingFraction $ 0 % 1
leadingFraction _ (LeadingCoefficient 0) =
    LeadingFraction $ 0 % 1
leadingFraction (LeadingCoefficient cMin) (LeadingCoefficient c) =
    LeadingFraction $ max (0 % 1) lf
    where
        numerator = fromRational $ c - cMin :: Double
        denominator = leadingDenominator cMin
        frac = (numerator / denominator) ** (2 / 3)
        lf = (1 % 1) - toRational frac

allZero :: [LcTrack] -> [LeadingFraction]
allZero tracks = const (LeadingFraction $ 0 % 1) <$> tracks

-- | Calculate the leading factor for all tracks.
leadingFractions :: TaskDeadline -> LengthOfSs -> [LcTrack] -> [LeadingFraction]
leadingFractions (TaskDeadline 0) _ tracks =
    allZero tracks
leadingFractions _ (LengthOfSs 0) tracks =
    allZero tracks
leadingFractions deadlines lens tracks =
    if cMin == 0 || leadingDenominator cMin == 0
       then allZero tracks
       else leadingFraction (LeadingCoefficient cMin) <$> cs
    where
        cs = leadingCoefficients deadlines lens tracks
        csNonZero = filter (\(LeadingCoefficient x) -> x > 0) cs
        cMin =
            if null csNonZero then 0 % 1
                              else minimum $ (\(LeadingCoefficient x) -> x) <$> csNonZero

{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ScopedTypeVariables #-}
module Flight.Leading
    ( TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    , madeGoal
    , cleanTrack
    , leadingCoefficient
    , leadingFractions
    )where

import Data.Ratio ((%))
import Flight.Ratio (pattern (:%))
import Data.List (partition, sortBy)
import Data.Maybe (catMaybes)

-- | Time in seconds from the moment the first pilot crossed the start of the speed
-- section.
newtype TaskTime = TaskTime Rational deriving (Eq, Ord, Show)

-- | Task time when the last pilot made the end of the speed section.
newtype EssTime = EssTime Rational deriving (Eq, Ord, Show)

-- | Time in seconds for the close of the task. 
-- section.
newtype TaskDeadline = TaskDeadline Rational deriving (Eq, Ord, Show)

-- | The distance in km to the end of the speed section.
newtype DistanceToEss = DistanceToEss Rational deriving (Eq, Ord, Show)

-- | The length of the speed section in km.
newtype LengthOfSs = LengthOfSs Rational deriving (Eq, Ord, Show)

newtype LeadingCoefficient = LeadingCoefficient Rational deriving (Eq, Ord, Show)

-- | A pilot's track where points are task time paired with distance to the end
-- of the speed section.
newtype LcTrack = LcTrack [(TaskTime, DistanceToEss)] deriving (Eq, Ord, Show)

newtype LeadingFraction = LeadingFraction Rational deriving (Eq, Ord, Show)

madeGoal :: LcTrack -> Bool
madeGoal (LcTrack xs) =
    any (\(DistanceToEss d) -> d <= 0) $ snd <$> xs

-- | Removes points where the task time < 1.
positiveTime :: LcTrack -> LcTrack
positiveTime (LcTrack xs) =
    LcTrack $ filter (\(TaskTime t, _) -> t > 0) xs

-- | Removes points where the distance to ESS increases.
towardsGoal :: LcTrack -> LcTrack
towardsGoal (LcTrack xs)
    | null xs =
        LcTrack xs
    | otherwise =
        LcTrack $ catMaybes $ zipWith f (zero : xs) xs
        where
            (TaskTime tN, dist) = head xs
            zero = (TaskTime $ tN - (1 % 1), dist)
            f (_, dM) x@(_, dN) = if dM < dN then Nothing else Just x

-- | Removes initial points further from ESS than the course length.
initialOffside :: LengthOfSs -> LcTrack -> LcTrack
initialOffside (LengthOfSs len) (LcTrack xs) =
    LcTrack $ dropWhile (\(_, DistanceToEss d) -> d > len) xs

cleanTrack :: LengthOfSs -> LcTrack -> LcTrack
cleanTrack len = towardsGoal . positiveTime . (initialOffside len) 

-- | Calculate the leading coefficient for a single track.
leadingCoefficient :: TaskDeadline
                      -> LengthOfSs
                      -> LcTrack
                      -> LeadingCoefficient
leadingCoefficient _ (LengthOfSs (0 :% _)) _ =
    LeadingCoefficient $ 0 % 1
leadingCoefficient _ _ (LcTrack []) =
    LeadingCoefficient $ 0 % 1
leadingCoefficient (TaskDeadline deadline) (LengthOfSs len) track
    | deadline <= 1 =
        LeadingCoefficient $ 0 % 1
    | otherwise =
        LeadingCoefficient $ sectionSum * (1 % 1800) * (d % n)
        where
            zero :: (TaskTime, DistanceToEss)
            zero = (TaskTime 0, DistanceToEss len)

            clampToDeadline :: LcTrack -> LcTrack
            clampToDeadline (LcTrack xsTrack) =
                LcTrack (clamp <$> xsTrack)
                where
                    clamp (TaskTime t, dist) = (TaskTime $ min deadline t, dist)

            withinDeadline :: LcTrack
            withinDeadline = clampToDeadline track

            ys :: [(TaskTime, DistanceToEss)]
            ys = (\(LcTrack xs) -> xs) withinDeadline

            f :: (TaskTime, DistanceToEss) -> (TaskTime, DistanceToEss) -> Rational
            f (TaskTime _, DistanceToEss dM) (TaskTime tN, DistanceToEss dN) =
                tN * dM * dM - dN * dN

            sectionSum :: Rational
            sectionSum = sum (zipWith f (zero : ys) $ ys)

            (n :% d) = len * len

-- | Calculate the leading coefficient for all tracks.
leadingCoefficients :: TaskDeadline
                      -> LengthOfSs
                      -> [LcTrack]
                      -> [LeadingCoefficient]
leadingCoefficients deadline@(TaskDeadline maxTaskTime) len tracks =
    snd <$> csSorted
    where
        cleanXs :: [LcTrack]
        cleanXs = (cleanTrack len) <$> tracks

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
                safeLast (_, LcTrack xs) =
                    if null xs then Nothing else Just (fst $ last xs)

        (TaskTime essTime) =
            if null essTimes then (TaskTime maxTaskTime) else maximum essTimes

        (xsEarly :: [(Int, LcTrack)], xsLate :: [(Int, LcTrack)]) =
            partition
                (\(_, LcTrack xs) ->
                    all (\(TaskTime t, _) -> t < essTime) xs)
                xsLandedOut

        lc :: LcTrack -> LeadingCoefficient
        lc = leadingCoefficient deadline len

        csMadeGoal :: [(Int, LeadingCoefficient)]
        csMadeGoal = (\(i, x) -> (i, lc x)) <$> xsMadeGoal

        lcE :: LcTrack -> LeadingCoefficient
        lcE track@(LcTrack xs) =
            if null xs then LeadingCoefficient $ 0 % 1 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b = (\(_, DistanceToEss d) -> essTime * d * d) $ last xs

        csEarly :: [(Int, LeadingCoefficient)]
        csEarly = (\(i, xs) -> (i, lcE xs)) <$> xsEarly

        lcL :: LcTrack -> LeadingCoefficient
        lcL track@(LcTrack xs) =
            if null xs then LeadingCoefficient $ 0 % 1 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b = (\(TaskTime t, DistanceToEss d) -> t * d * d) $ last xs

        csLate :: [(Int, LeadingCoefficient)]
        csLate = (\(i, xs) -> (i, lcL xs)) <$> xsLate

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
allZero tracks = (const (LeadingFraction $ 0 % 1)) <$> tracks

-- | Calculate the leading factor for all tracks.
leadingFractions :: TaskDeadline -> LengthOfSs -> [LcTrack] -> [LeadingFraction]
leadingFractions (TaskDeadline 0) _ tracks =
    allZero tracks
leadingFractions _ (LengthOfSs 0) tracks =
    allZero tracks
leadingFractions deadlines lens tracks =
    if cMin == 0 || leadingDenominator cMin == 0
       then allZero tracks
       else (leadingFraction (LeadingCoefficient cMin)) <$> cs
    where
        cs = leadingCoefficients deadlines lens tracks
        csNonZero = filter (\(LeadingCoefficient x) -> x > 0) cs
        cMin =
            if null csNonZero then (0 % 1)
                              else minimum $ (\(LeadingCoefficient x) -> x) <$> csNonZero

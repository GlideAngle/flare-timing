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
import "newtype" Control.Newtype (Newtype(..))
import Control.Arrow (second)
import Data.Ratio ((%))
import Data.List (partition, sortBy)
import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure ((-:), (*:), u, zero, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces)
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Units ()
import Flight.Ratio (pattern (:%))
import Flight.Gap.Ratio.Leading
    (LeadingAreaScaling(..), LeadingCoefficient(..), LeadingFraction(..))
import Flight.Gap.Area.Leading (LeadingAreaStep(..))
import Flight.Gap.Equation (powerFraction)

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

data LcSeq a =
    LcSeq
        { seq :: [a]
        , extra :: Maybe a
        }
    deriving (Eq, Ord, Show)

-- | A pilot's track where points are task time paired with distance to the end
-- of the speed section.
type LcTrack = LcSeq LcPoint

type LcArea = LcSeq (LeadingAreaStep (Quantity Double [u| (km^2)*s |]))

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

-- TODO: Log a case with uom-plugin
-- areaSteps _ (LengthOfSs [u| 0 km |]) LcSeq{seq = xs} =
--    • Couldn't match type ‘GHC.Real.Ratio Integer’ with ‘Integer’
--      Expected type: Quantity
--                       Rational (Data.UnitsOfMeasure.Internal.MkUnit "km")
--        Actual type: Quantity
--                       Integer (Data.UnitsOfMeasure.Internal.MkUnit "km")
--    • When checking that the pattern signature:
--          Quantity Integer (Data.UnitsOfMeasure.Internal.MkUnit "km")
--        fits the type of its context:
--          Quantity Rational (Data.UnitsOfMeasure.Internal.MkUnit "km")
--      In the pattern:
--        MkQuantity 0 :: Quantity Integer (Data.UnitsOfMeasure.Internal.MkUnit "km")
--      In the pattern:
--        LengthOfSs (MkQuantity 0 :: Quantity Integer (Data.UnitsOfMeasure.Internal.MkUnit "km"))

-- | Calculate the leading area for a single track.
areaSteps
    :: TaskDeadline
    -> LengthOfSs
    -> LcTrack
    -> LcArea
areaSteps _ (LengthOfSs [u| 0 % 1 km |]) LcSeq{seq = xs} =
    LcSeq
        { seq = const (LeadingAreaStep zero) <$> xs
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
    | dl <= [u| 1s |] =
        LcSeq
            { seq = const (LeadingAreaStep zero) <$> xs'
            , extra = Nothing
            }
    | otherwise =
        LcSeq
            { seq =
                LeadingAreaStep
                . (scale $ areaScaling len)
                <$> steps

            , extra =
                LeadingAreaStep
                . (scale $ areaScaling len)
                . g
                <$> extra
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

            f :: LcPoint -> LcPoint -> Quantity _ [u| (km^2)*s |]
            f
                LcPoint
                    { mark = TaskTime tM
                    , togo = DistanceToEss dM
                    }
                LcPoint
                    { leg
                    , mark = TaskTime tN
                    , togo = DistanceToEss dN
                    }
                | tM == tN = zero
                | tN < zero = zero
                | not (isRaceLeg leg) = zero
                | otherwise = tN *: (dM *: dM -: dN *: dN)

            g :: LcPoint -> Quantity _ [u| (km^2)*s |]
            g LcPoint
                { mark = TaskTime t
                , togo = DistanceToEss d
                } = t *: d *: d

            steps :: [Quantity _ [u| (km^2)*s |]]
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
            p{togo = DistanceToEss $ max [u| 0 km |] dist}

scale
    :: LeadingAreaScaling
    -> Quantity Rational [u| (km^2)*s |]
    -> Quantity Double [u| (km^2)*s |]
scale (LeadingAreaScaling scaling) q =
    fromRational' $ q *: MkQuantity scaling

areaScaling :: LengthOfSs -> LeadingAreaScaling
areaScaling (LengthOfSs (MkQuantity len)) =
    let (n :% d) = len * len in LeadingAreaScaling $ (1 % 1800) * (d % n)

-- | Calculate the leading coefficient for a single track.
leadingCoefficient
    :: TaskDeadline
    -> LengthOfSs
    -> LcTrack
    -> LeadingCoefficient
leadingCoefficient deadline len xs =
    LeadingCoefficient . toRational . sum
    $ (\(LeadingAreaStep (MkQuantity x)) -> x)
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

        (TaskTime tEss@(MkQuantity essTime)) =
            if null essTimes then TaskTime maxTaskTime else maximum essTimes

        (xsEarly :: [(Int, LcTrack)], xsLate :: [(Int, LcTrack)]) =
            partition
                (\(_, LcSeq{seq = xs}) ->
                    all (\LcPoint{mark = TaskTime t} -> t < tEss) xs)
                xsLandedOut

        lc :: LcTrack -> LeadingCoefficient
        lc = leadingCoefficient deadline len

        csMadeGoal :: [(Int, LeadingCoefficient)]
        csMadeGoal = second lc <$> xsMadeGoal

        lcE :: LcTrack -> LeadingCoefficient
        lcE track@LcSeq{seq = xs} =
            if null xs then LeadingCoefficient 0 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b =
                    (\LcPoint{togo = DistanceToEss (MkQuantity d)} ->
                        essTime * d * d)
                    $ last xs

        csEarly :: [(Int, LeadingCoefficient)]
        csEarly = second lcE <$> xsEarly

        lcL :: LcTrack -> LeadingCoefficient
        lcL track@LcSeq{seq = xs} =
            if null xs then LeadingCoefficient 0 else
            LeadingCoefficient $ a + b
            where
                (LeadingCoefficient a) = lc track
                b =
                    (\LcPoint{ mark = TaskTime (MkQuantity t)
                             , togo = DistanceToEss (MkQuantity d)
                             } -> t * d * d)
                    $ last xs

        csLate :: [(Int, LeadingCoefficient)]
        csLate = second lcL <$> xsLate

        csMerged :: [(Int, LeadingCoefficient)]
        csMerged = mconcat [ csMadeGoal, csEarly, csLate ]

        csSorted :: [(Int, LeadingCoefficient)]
        csSorted = sortBy (\x y -> fst x `compare` fst y) csMerged

leadingDenominator :: Rational -> Double
leadingDenominator cMin = fromRational cMin ** (1/2)

leadingFraction
    :: LeadingCoefficient
    -> LeadingCoefficient
    -> LeadingFraction
leadingFraction (LeadingCoefficient lcMin) (LeadingCoefficient lc) =
    LeadingFraction . toRational $ powerFraction lcMin' lc'
    where
        lcMin' = fromRational lcMin
        lc' = fromRational lc

allZero :: [LcTrack] -> [LeadingFraction]
allZero tracks = const (LeadingFraction 0) <$> tracks

-- | Calculate the leading factor for all tracks.
leadingFractions :: TaskDeadline -> LengthOfSs -> [LcTrack] -> [LeadingFraction]
leadingFractions (TaskDeadline (MkQuantity 0)) _ tracks =
    allZero tracks
leadingFractions _ (LengthOfSs (MkQuantity 0)) tracks =
    allZero tracks
leadingFractions deadlines lens tracks =
    if cMin == 0 || leadingDenominator cMin == 0
       then allZero tracks
       else leadingFraction (LeadingCoefficient cMin) <$> cs
    where
        cs = leadingCoefficients deadlines lens tracks
        csNonZero = filter (\(LeadingCoefficient x) -> x > 0) cs
        cMin =
            if null csNonZero then 0 else
                minimum $ (\(LeadingCoefficient x) -> x) <$> csNonZero

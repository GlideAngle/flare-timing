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
    , areaToCoef
    , areaSteps
    , mkCoef
    , leadingCoefficient
    , leadingFraction
    , leadingFractions
    , clampToEss
    , clampToDeadline
    , showSecs
    ) where

import Prelude hiding (seq)
import "newtype" Control.Newtype (Newtype(..))
import Control.Arrow (second)
import Data.List (partition, sortBy)
import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure
    ( (-:), (*:), (+:)
    , u, zero, fromRational', toRational', recip'
    )
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces)
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Units ()
import Flight.Gap.Ratio.Leading (LeadingFraction(..))
import Flight.Gap.Leading.Area (LeadingArea(..), LeadingAreaUnits)
import Flight.Gap.Leading.Coef (LeadingCoef(..), LeadingCoefUnits, zeroLeadingCoefUnits)
import Flight.Gap.Leading.Scaling (AreaToCoef(..), AreaToCoefUnits)
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

type LcArea = LcSeq (LeadingArea LeadingAreaUnits)
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
        { seq = const (LeadingArea zero) <$> xs
        , extra = Nothing
        }

areaSteps _ _ LcSeq{seq = []} =
    LcSeq
        { seq = []
        , extra = Nothing
        }

areaSteps
    deadline@(TaskDeadline dl)
    _
    track@LcSeq{seq = xs', extra}
    | dl <= [u| 1s |] =
        LcSeq
            { seq = const (LeadingArea zero) <$> xs'
            , extra = Nothing
            }
    | otherwise =
        LcSeq
            { seq =
                LeadingArea
                . fromRational'
                <$> steps

            , extra =
                LeadingArea
                . fromRational'
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

mkCoef
    :: AreaToCoef AreaToCoefUnits
    -> Quantity Rational [u| (km^2)*s |]
    -> Quantity Double [u| 1 |]
mkCoef (AreaToCoef k) area = fromRational' $ k *: area

areaToCoef :: LengthOfSs -> AreaToCoef AreaToCoefUnits
areaToCoef (LengthOfSs l) =
    AreaToCoef . recip' $ [u| 1800 s |] *: l *: l

-- | Calculate the leading coefficient for a single track.
leadingCoefficient
    :: TaskDeadline
    -> LengthOfSs
    -> LcTrack
    -> LeadingCoef LeadingCoefUnits
leadingCoefficient deadline len xs =
    LeadingCoef $ sum'
    $ f
    <$> seq (areaSteps deadline len xs)
    where
        sum' = foldr (+:) zero

        k :: Quantity Rational [u| (km^2)*s |] -> Quantity Double [u| 1 |]
        k x = mkCoef (areaToCoef len) x

        f :: LeadingArea LeadingAreaUnits -> LeadingCoefUnits
        f (LeadingArea a) = k $ toRational' a

-- | Calculate the leading coefficient for all tracks.
leadingCoefficients
    :: TaskDeadline
    -> LengthOfSs
    -> [LcTrack]
    -> [LeadingCoef LeadingCoefUnits]
leadingCoefficients deadline@(TaskDeadline maxTaskTime) len tracks =
    snd <$> csSorted
    where
        k :: Quantity Rational [u| (km^2)*s |] -> Quantity Double [u| 1 |]
        k x = mkCoef (areaToCoef len) x

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

        lc :: LcTrack -> LeadingCoef LeadingCoefUnits
        lc = leadingCoefficient deadline len

        csMadeGoal :: [(Int, LeadingCoef LeadingCoefUnits)]
        csMadeGoal = second lc <$> xsMadeGoal

        lcE :: LcTrack -> LeadingCoef LeadingCoefUnits
        lcE track@LcSeq{seq = xs} =
            if null xs then LeadingCoef zero else
            LeadingCoef $ a +: b
            where
                (LeadingCoef a) = lc track

                b :: Quantity _ [u| 1 |]
                b =
                    (\LcPoint{togo = DistanceToEss d} ->
                        let area :: Quantity Rational [u| (km^2)*s |]
                            area = MkQuantity essTime *: d *: d

                        in k area)
                    $ last xs

        csEarly :: [(Int, LeadingCoef LeadingCoefUnits)]
        csEarly = second lcE <$> xsEarly

        lcL :: LcTrack -> LeadingCoef LeadingCoefUnits
        lcL track@LcSeq{seq = xs} =
            if null xs then LeadingCoef zero else
            LeadingCoef $ a +: b
            where
                (LeadingCoef a) = lc track
                b =
                    (\LcPoint{ mark = TaskTime t
                             , togo = DistanceToEss d
                             } ->
                        let area :: Quantity Rational [u| (km^2)*s |]
                            area = t *: d *: d

                        in k area)
                    $ last xs

        csLate :: [(Int, LeadingCoef LeadingCoefUnits)]
        csLate = second lcL <$> xsLate

        csMerged :: [(Int, LeadingCoef LeadingCoefUnits)]
        csMerged = mconcat [csMadeGoal, csEarly, csLate]

        csSorted :: [(Int, LeadingCoef LeadingCoefUnits)]
        csSorted = sortBy (\x y -> fst x `compare` fst y) csMerged

leadingDenominator :: LeadingCoefUnits -> Double
leadingDenominator (MkQuantity cMin) = cMin ** (1/2)

leadingFraction
    :: LeadingCoef LeadingCoefUnits
    -> LeadingCoef LeadingCoefUnits
    -> LeadingFraction
leadingFraction (LeadingCoef (MkQuantity lcMin)) (LeadingCoef (MkQuantity lc)) =
    LeadingFraction . toRational $ powerFraction lcMin lc

allZero :: [LcTrack] -> [LeadingFraction]
allZero tracks = const (LeadingFraction 0) <$> tracks

-- | Calculate the leading factor for all tracks.
leadingFractions
    :: TaskDeadline
    -> LengthOfSs
    -> [LcTrack]
    -> [LeadingFraction]
leadingFractions (TaskDeadline (MkQuantity 0)) _ tracks =
    allZero tracks
leadingFractions _ (LengthOfSs (MkQuantity 0)) tracks =
    allZero tracks
leadingFractions deadlines lens tracks =
    if | cMin == zeroLeadingCoefUnits -> allZero tracks
       | leadingDenominator cMin == 0 -> allZero tracks
       | otherwise -> leadingFraction (LeadingCoef cMin) <$> cs
    where
        cs :: [LeadingCoef LeadingCoefUnits]
        cs = leadingCoefficients deadlines lens tracks

        csNonZero :: [LeadingCoef LeadingCoefUnits]
        csNonZero = filter gtZero cs

        cMin :: LeadingCoefUnits
        cMin =
            if null csNonZero then zeroLeadingCoefUnits else
                minimum $ (\(LeadingCoef x) -> x) <$> csNonZero

        gtZero :: LeadingCoef LeadingCoefUnits -> Bool
        gtZero (LeadingCoef (MkQuantity x)) = x > 0

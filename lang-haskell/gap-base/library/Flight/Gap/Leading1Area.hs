module Flight.Gap.Leading1Area
    ( area1Steps
    , mk1Coef
    , area1toCoef
    ) where

import Prelude hiding (seq)
import Data.UnitsOfMeasure ((-:), (*:), u, zero, fromRational', recip')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Gap.Fraction.Leading (EssTime(..), LeadAllDown(..))
import Flight.Gap.Leading.Area
    ( LeadingAreas(..), LeadingArea(..)
    , LeadingArea1Units, zeroLeadingArea1Units
    )
import Flight.Gap.Leading.Scaling (AreaToCoef(..), Area1ToCoefUnits)
import Flight.Gap.Leading

mk1Coef
    :: AreaToCoef Area1ToCoefUnits
    -> Quantity Rational [u| km*s |]
    -> Quantity Double [u| 1 |]
mk1Coef (AreaToCoef k) area = fromRational' $ k *: area

area1toCoef :: LengthOfSs -> AreaToCoef Area1ToCoefUnits
area1toCoef (LengthOfSs l) =
    AreaToCoef . recip' $ [u| 1800 s |] *: l

-- TODO: Log a case with uom-plugin
-- area1Steps _ (LengthOfSs [u| 0 km |]) LcSeq{seq = xs} =
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
area1Steps
    :: TaskDeadline
    -> LeadAllDown
    -> LengthOfSs
    -> LcTrack
    -> LcArea LeadingArea1Units
area1Steps _ _ (LengthOfSs [u| 0 % 1 km |]) LcSeq{seq = xs} =
    LeadingAreas
        { areaFlown = LcSeq{seq = const (LeadingArea zero) <$> xs}
        , areaAfterLanding = LeadingArea zeroLeadingArea1Units
        , areaBeforeStart = LeadingArea zeroLeadingArea1Units
        }

area1Steps _ _ _ LcSeq{seq = []} =
    LeadingAreas
        { areaFlown = LcSeq{seq = []}
        , areaAfterLanding = LeadingArea zeroLeadingArea1Units
        , areaBeforeStart = LeadingArea zeroLeadingArea1Units
        }

area1Steps
    deadline@(TaskDeadline dl)
    (LeadAllDown (EssTime tEss))
    (LengthOfSs lenOfSs)
    track@LcSeq{seq = xs'}
    | dl <= [u| 1s |] =
        LeadingAreas
            { areaFlown = LcSeq{seq = const (LeadingArea zero) <$> xs'}
            , areaAfterLanding = LeadingArea zeroLeadingArea1Units
            , areaBeforeStart = LeadingArea zeroLeadingArea1Units
            }
    | otherwise =
        LeadingAreas
            { areaFlown = LcSeq{seq = LeadingArea . fromRational' <$> steps}
            , areaAfterLanding = afterLanding
            , areaBeforeStart = beforeStart
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

            stepArea :: LcPoint -> LcPoint -> Quantity _ [u| km*s |]
            stepArea
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
                | otherwise = tN *: (dM -: dN)

            afterArea :: LcPoint -> Quantity _ [u| km*s |]
            afterArea LcPoint{mark = TaskTime t@(MkQuantity ticks), togo = DistanceToEss d} =
                if ticks == tEss then zero else t *: d

            beforeArea :: LcPoint -> Quantity _ [u| km*s |]
            beforeArea LcPoint{mark = TaskTime t} =
                t *: lenOfSs

            steps :: [Quantity _ [u| km*s |]]
            steps = zipWith stepArea ys' ys

            afterLanding =
                case reverse ys of
                    (y : _) -> LeadingArea . fromRational' $ afterArea y
                    _ -> LeadingArea zeroLeadingArea1Units

            beforeStart =
                case ys of
                    (y : _) -> LeadingArea . fromRational' $ beforeArea y
                    _ -> LeadingArea zeroLeadingArea1Units

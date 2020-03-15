module Flight.Gap.Leading2Area
    ( area2Steps
    ) where

import Prelude hiding (seq)
import Data.UnitsOfMeasure
    ( (-:), (*:)
    , u, zero, fromRational'
    )
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Gap.Fraction.Leading (EssTime(..), LeadAllDown(..))
import Flight.Gap.Leading.Area
    ( LeadingAreas(..), LeadingArea(..)
    , LeadingArea2Units, zeroLeadingArea2Units
    )
import Flight.Gap.Leading

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
area2Steps
    :: TaskDeadline
    -> LeadAllDown
    -> LengthOfSs
    -> LcTrack
    -> LcArea LeadingArea2Units
area2Steps _ _ (LengthOfSs [u| 0 % 1 km |]) LcSeq{seq = xs} =
    LeadingAreas
        { areaFlown = LcSeq{seq = const (LeadingArea zero) <$> xs}
        , areaAfterLanding = LeadingArea zeroLeadingArea2Units
        , areaBeforeStart = LeadingArea zeroLeadingArea2Units
        }

area2Steps _ _ _ LcSeq{seq = []} =
    LeadingAreas
        { areaFlown = LcSeq{seq = []}
        , areaAfterLanding = LeadingArea zeroLeadingArea2Units
        , areaBeforeStart = LeadingArea zeroLeadingArea2Units
        }

area2Steps
    deadline@(TaskDeadline dl)
    (LeadAllDown (EssTime tEss))
    (LengthOfSs lenOfSs)
    track@LcSeq{seq = xs'}
    | dl <= [u| 1s |] =
        LeadingAreas
            { areaFlown = LcSeq{seq = const (LeadingArea zero) <$> xs'}
            , areaAfterLanding = LeadingArea zeroLeadingArea2Units
            , areaBeforeStart = LeadingArea zeroLeadingArea2Units
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

            stepArea :: LcPoint -> LcPoint -> Quantity _ [u| (km^2)*s |]
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
                | otherwise = tN *: (dM *: dM -: dN *: dN)

            afterArea :: LcPoint -> Quantity _ [u| (km^2)*s |]
            afterArea LcPoint{mark = TaskTime t@(MkQuantity ticks), togo = DistanceToEss d} =
                if ticks == tEss then zero else t *: d *: d

            beforeArea :: LcPoint -> Quantity _ [u| (km^2)*s |]
            beforeArea LcPoint{mark = TaskTime t} =
                t *: lenOfSs *: lenOfSs

            steps :: [Quantity _ [u| (km^2)*s |]]
            steps = zipWith stepArea ys' ys

            afterLanding =
                case reverse ys of
                    (y : _) -> LeadingArea . fromRational' $ afterArea y
                    _ -> LeadingArea zeroLeadingArea2Units

            beforeStart =
                case ys of
                    (y : _) -> LeadingArea . fromRational' $ beforeArea y
                    _ -> LeadingArea zeroLeadingArea2Units

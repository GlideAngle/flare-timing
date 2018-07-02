{-# LANGUAGE RankNTypes #-}

module Flight.Mask.Internal.Race
    ( FlyClipping(..)
    , FlyClipSection(..)
    , Sliver(..)
    , Ticked
    , RaceSections(..)
    , Reach
    , FlyCut(..)
    , section
    , cons
    , mm30
    ) where

import Data.Ratio ((%))

import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.List (findIndices)

import Flight.Distance (SpanLatLng, TaskDistance(..))
import Flight.Kml (FixMark(mark), MarkedFixes(..), Seconds(..))
import Flight.Zone (Zone(..))
import Flight.Zone.Cylinder (CircumSample, Tolerance(..))
import Flight.Track.Time (TimeRow(..))
import Flight.Comp (SpeedSection, FlyingSection)
import Flight.Units ()
import Flight.Task (CostSegment, DistancePointToPoint, AngleCut(..))
import Flight.Mask.Internal.Zone (ZoneIdx, TaskZone(..), TrackZone(..))
import Flight.Mask.Internal.Cross (CrossingPredicate)

mm30 :: Fractional a => Tolerance a
mm30 = Tolerance . fromRational $ 30 % 1000

-- | When working out distances around a course, if I know which zones are
-- tagged then I can break up the track into legs and assume previous legs are
-- ticked when working out distance to goal.
type Ticked = RaceSections ZoneIdx

data RaceSections a =
    RaceSections
        { prolog :: [a]
        -- ^ Zones crossed before the start of the speed section.
        , race :: [a]
        -- ^ Zones crossed during the speed section.
        , epilog :: [a]
        -- ^ Zones crossed after the end of the speed section.
        }
        deriving Show

data Sliver a =
    Sliver
        { span :: SpanLatLng a
        , dpp :: DistancePointToPoint a
        , cseg :: CostSegment a
        , cs :: CircumSample a
        , angleCut :: AngleCut a
        }

-- | Without know which zones have been ticked, a function that uses a crossing
-- predicate to work out a distance.
type Reach a b c
    = (a -> TrackZone b)
    -> SpeedSection
    -> [CrossingPredicate b c]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)

-- | Slice a list into three parts, before, during and after the speed section.
section :: SpeedSection -> [a] -> RaceSections a 

section Nothing xs =
    RaceSections 
        { prolog = []
        , race = xs
        , epilog = []
        }

section (Just (s', e')) xs =
    RaceSections 
        { prolog = take s xs
        , race = take (e - s + 1) . drop s $ xs
        , epilog = drop (e + 1) xs
        }
    where
        (s, e) = (s' - 1, e' - 1)

cons :: (a -> TrackZone b) -> a -> [Zone b] -> [Zone b]
cons mkZone x zs = unTrackZone (mkZone x) : zs

-- | The subset of the fixes that can be considered flown.
data FlyCut a b =
    FlyCut
        { cut :: FlyingSection a
        , uncut :: b
        }
        deriving Show

class FlyClipping a b where
    clipToFlown :: FlyCut a b -> FlyCut a b
    clipIndices :: FlyCut a b -> [Int]

class (FlyClipping a b) => FlyClipSection a b c where
    clipSection :: FlyCut a b -> FlyingSection c

instance FlyClipping UTCTime [TimeRow] where
    clipToFlown x@FlyCut{cut = Nothing} =
        x{uncut = []}

    clipToFlown x@FlyCut{cut = Just (t0, t1), uncut = xs} =
        x{uncut = filter (betweenTimeRow t0 t1) xs}

    clipIndices FlyCut{cut = Nothing} = []

    clipIndices FlyCut{cut = Just (t0, t1), uncut = xs} =
        findIndices (betweenTimeRow t0 t1) xs

instance FlyClipping UTCTime MarkedFixes where
    clipToFlown x@FlyCut{cut = Nothing, uncut} =
        x{uncut = uncut{fixes = []}}

    clipToFlown x@FlyCut{cut = Just (t0, t1), uncut = mf@MarkedFixes{mark0, fixes}} =
        x{uncut = mf{fixes = filter (betweenFixMark s0 s1) fixes}}
        where
            s0 = Seconds . round $ t0 `diffUTCTime` mark0
            s1 = Seconds . round $ t1 `diffUTCTime` mark0

    clipIndices FlyCut{cut = Nothing} = []

    clipIndices FlyCut{cut = Just (t0, t1), uncut = MarkedFixes{mark0, fixes}} =
        findIndices (betweenFixMark s0 s1) fixes
        where
            s0 = Seconds . round $ t0 `diffUTCTime` mark0
            s1 = Seconds . round $ t1 `diffUTCTime` mark0

instance FlyClipSection UTCTime MarkedFixes Int where
    clipSection x =
        case (xs, reverse xs) of
            ([], _) -> Nothing
            (_, []) -> Nothing
            (i : _, j : _) -> Just (i, j)
        where
            xs = clipIndices x

betweenTimeRow :: UTCTime -> UTCTime -> TimeRow -> Bool
betweenTimeRow t0 t1 TimeRow{time = t} =
    t0 <= t && t <= t1 

betweenFixMark :: FixMark a => Seconds -> Seconds -> a -> Bool
betweenFixMark s0 s1 x =
    let s = mark x in s0 <= s && s <= s1

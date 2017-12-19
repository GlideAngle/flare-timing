{-# LANGUAGE RankNTypes #-}

module Flight.Mask.Internal.Race
    ( Sliver(..)
    , Ticked
    , RaceSections(..)
    , Reach
    , section
    , cons
    , mm30
    ) where

import Prelude hiding (span)
import Data.Ratio ((%))

import Flight.Zone (Zone(..))
import Flight.Comp (SpeedSection)
import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import Flight.Task
    ( Tolerance(..)
    , SpanLatLng
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , CircumSample
    )
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

data Sliver a =
    Sliver
        { span :: SpanLatLng a
        , dpp :: DistancePointToPoint a
        , cseg :: CostSegment a
        , cs :: CircumSample a
        , cut :: AngleCut a
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
        (s, e) = (fromInteger s' - 1, fromInteger e' - 1)

cons :: (a -> TrackZone b) -> a -> [Zone b] -> [Zone b]
cons mkZone x zs = unTrackZone (mkZone x) : zs

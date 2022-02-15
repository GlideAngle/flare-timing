module Flight.Mask.Internal.Race
    ( FlyClipSection(..)
    , Ticked
    , RaceSections(..)
    , Reach
    , FlyCut(..)
    , section
    , mm30
    ) where

import Data.Ratio ((%))

import Flight.Clip (FlyCut(..), FlyClipSection(..))
import Flight.Distance (TaskDistance(..))
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Zone.Cylinder (Tolerance(..))
import Flight.Track.Time (ZoneIdx(..))
import Flight.Units ()
import Flight.Mask.Internal.Zone (TaskZone(..), TrackZone(..))
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

-- | Without knowing which zones have been ticked, a function that uses a
-- crossing predicate to work out a distance.
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

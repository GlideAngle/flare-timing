module Flight.Mask.Group (groupByLeg) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.List (nub)
import Data.List.Split (split, whenElt, keepDelimsL)
import qualified Data.Map as Map

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Distance (SpanLatLng)
import Flight.Kml (MarkedFixes(..), fixToUtc)
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (Fix(..))
import Flight.Track.Time (LegIdx(..))
import Flight.Comp (Task(..))
import Flight.Units ()
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , TaskZone(..)
    )
import Flight.Mask.Tag (tagZones, madeZones)
import qualified Flight.Zone.Raw as Raw (RawZone(..))

-- | Groups fixes by legs of the task.
-- >>> let az = ['a' .. 'z']
--
-- >>> az
-- "abcdefghijklmnopqrstuvwxyz"
--
-- >>> let ez = drop 4 az
--
-- >>> ez
-- "efghijklmnopqrstuvwxyz"
--
-- >>> let vowels = Just <$> ['a', 'e', 'i', 'o', 'u']
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) az
-- ["","abcd","efgh","ijklmn","opqrst","uvwxyz"]
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) ez
-- ["","efgh","ijklmn","opqrst","uvwxyz"]
--
-- WARNING: The list is expected to have no duplicates.
-- >>> let aooz = ['a' .. 'o'] ++ ['o' .. 'z']
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) aooz
-- ["","abcd","efgh","ijklmn","o","opqrst","uvwxyz"]
groupByLeg
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => SpanLatLng a
    -> (Raw.RawZone -> TaskZone a)
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> [(LegIdx, MarkedFixes)]
groupByLeg span zoneToCyl task flyCut =
    [ (leg, mf{fixes = ys})
    | ys <- yss
    , let leg =
            case ys of
                [] -> LegIdx 0
                (fix : _) ->
                    maybe
                        (LegIdx 0)
                        id
                        (Map.lookup (Just $ fixToUtc mark0 fix) timeToLeg)
    ]
    where
        FlyCut{uncut = mf@MarkedFixes{mark0, fixes}} = clipToFlown flyCut

        xs :: [Maybe Fix]
        xs =
            tagZones . unSelectedCrossings . selectedCrossings
            $ madeZones span zoneToCyl task mf

        ts :: [Maybe UTCTime]
        ts = (fmap . fmap) time xs

        timeToLeg :: Map.Map (Maybe UTCTime) LegIdx
        timeToLeg = Map.fromList $ zip ts (LegIdx <$> [1..])

        -- WARNING: Pilots can end up with duplicate timestamps when they are
        -- logging at a sub-second rate. For IGC files the HMS and ss fields are
        -- in the same B record but in different locations. If the parser does
        -- not know about the sub-second field then it will parse multiple fixes
        -- with the same HMS time.
        uniqueFixes = nub fixes

        yss :: [[Kml.Fix]]
        yss =
            split
                (keepDelimsL
                $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                uniqueFixes

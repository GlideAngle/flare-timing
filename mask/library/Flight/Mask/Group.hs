module Flight.Mask.Group (groupByLeg) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.List (nub)
import Data.List.Split (split, whenElt, keepDelimsL)

import Flight.Distance (SpanLatLng)
import Flight.Kml (MarkedFixes(..), fixToUtc)
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (Fix(..))
import Flight.Track.Mask (FlyClipping(..), FlyCut(..))
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
    -> [MarkedFixes]
groupByLeg span zoneToCyl task flyCut =
    (\zs -> mf{fixes = zs}) <$> ys
    where
        FlyCut{uncut = mf@MarkedFixes{mark0, fixes}} = clipToFlown flyCut

        xs :: [Maybe Fix]
        xs =
            tagZones . unSelectedCrossings . selectedCrossings
            $ madeZones span zoneToCyl task mf

        ts :: [Maybe UTCTime]
        ts = (fmap . fmap) time xs

        -- WARNING: Pilots can end up with duplicate timestamps when they are
        -- logging at a sub-second rate. For IGC files the HMS and ss fields are
        -- in the same B record but in different locations. If the parser does
        -- not know about the sub-second field then it will parse multiple fixes
        -- with the same HMS time.
        uniqueFixes = nub fixes

        ys :: [[Kml.Fix]]
        ys =
            split
                (keepDelimsL
                $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                uniqueFixes

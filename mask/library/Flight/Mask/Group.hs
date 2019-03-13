module Flight.Mask.Group (GroupLeg(..), groupByLeg) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.These
import Data.List (nub)
import Data.List.Split (split, whenElt, keepDelimsL, keepDelimsR)
import qualified Data.Map as Map

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Kml (MarkedFixes(..), fixToUtc)
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (InterpolatedFix(..), ZoneTag(..))
import Flight.Track.Time (LegIdx(..))
import Flight.Comp (Task(..), Zones(..))
import Flight.Units ()
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , TaskZone(..)
    )
import Flight.Mask.Interpolate (TagInterpolate(..))
import Flight.Mask.Tag (tagZones, madeZones)
import qualified Flight.Zone.Raw as Raw (RawZone(..))

data GroupLeg =
    GroupLeg
        { groupLeg :: These LegIdx LegIdx
        , groupTime :: These UTCTime UTCTime
        }

-- | Groups fixes by legs of the task.
--
-- If the letters are a pilots fixes in sequence.
-- >>> let az = ['a' .. 'z']
--
-- >>> az
-- "abcdefghijklmnopqrstuvwxyz"
--
-- If the vowels are the fixes where the pilot tagged turnpoints then we can
-- group the fixes using split.
-- >>> let vowels = Just <$> ['a', 'e', 'i', 'o', 'u']
--
-- Keep and prepend delimiters with @keepDelimsL@.
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) az
-- ["","abcd","efgh","ijklmn","opqrst","uvwxyz"]
--
-- Keep and append delimiters with @keepDelimsR@.
-- >>> split (keepDelimsR $ whenElt (\x -> elem (Just x) vowels)) az
-- ["a","bcde","fghi","jklmno","pqrstu","vwxyz"]
--
-- Default behaviour is to keep delimiters as separate chunks.
-- >>> split (whenElt (\x -> elem (Just x) vowels)) az
-- ["","a","bcd","e","fgh","i","jklmn","o","pqrst","u","vwxyz"]
--
-- Some pilots don't ever tick the launch cylinder. We can model this by
-- excluding the letter a, the initial tagging.
-- >>> let bz = drop 1 az
-- >>> let dz = drop 3 az
-- >>> let ez = drop 4 az
--
-- >>> bz
-- "bcdefghijklmnopqrstuvwxyz"
-- >>> dz
-- "defghijklmnopqrstuvwxyz"
-- >>> ez
-- "efghijklmnopqrstuvwxyz"
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) bz
-- ["bcd","efgh","ijklmn","opqrst","uvwxyz"]
--
-- >>> split (keepDelimsR $ whenElt (\x -> elem (Just x) vowels)) bz
-- ["bcde","fghi","jklmno","pqrstu","vwxyz"]
--
-- >>> split (whenElt (\x -> elem (Just x) vowels)) bz
-- ["bcd","e","fgh","i","jklmn","o","pqrst","u","vwxyz"]
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) dz
-- ["d","efgh","ijklmn","opqrst","uvwxyz"]
--
-- >>> split (keepDelimsR $ whenElt (\x -> elem (Just x) vowels)) dz
-- ["de","fghi","jklmno","pqrstu","vwxyz"]
--
-- >>> split (whenElt (\x -> elem (Just x) vowels)) dz
-- ["d","e","fgh","i","jklmn","o","pqrst","u","vwxyz"]
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) ez
-- ["","efgh","ijklmn","opqrst","uvwxyz"]
--
-- >>> split (keepDelimsR $ whenElt (\x -> elem (Just x) vowels)) ez
-- ["e","fghi","jklmno","pqrstu","vwxyz"]
--
-- >>> split (whenElt (\x -> elem (Just x) vowels)) ez
-- ["","e","fgh","i","jklmn","o","pqrst","u","vwxyz"]
--
-- WARNING: The list is expected to have no duplicates.
-- >>> let aooz = ['a' .. 'o'] ++ ['o' .. 'z']
--
-- >>> split (keepDelimsL $ whenElt (\x -> elem (Just x) vowels)) aooz
-- ["","abcd","efgh","ijklmn","o","opqrst","uvwxyz"]
--
-- >>> split (keepDelimsR $ whenElt (\x -> elem (Just x) vowels)) aooz
-- ["a","bcde","fghi","jklmno","o","pqrstu","vwxyz"]
--
-- >>> split (whenElt (\x -> elem (Just x) vowels)) aooz
-- ["","a","bcd","e","fgh","i","jklmn","o","","o","pqrst","u","vwxyz"]
groupByLeg
    ::
        ( Real b
        , Fractional b
        , FlyClipping UTCTime MarkedFixes
        , TagInterpolate a b
        )
    => a
    -> (Raw.RawZone -> TaskZone b)
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> [(Maybe GroupLeg, MarkedFixes)]
groupByLeg tagInterp zoneToCyl task@Task{zones = Zones{raw = zs}} flyCut =
    [
        let g =
                case (nthR, zerothL) of
                (Nothing, Nothing) ->
                    Nothing
                (Nothing, Just (_, Nothing)) ->
                    Nothing
                (Just (_, Nothing), Nothing) ->
                    Nothing
                (Just (_, Nothing), Just (_, Nothing)) ->
                    Nothing
                (Just (tR, Just iR), Just (tL, Just iL)) ->
                    Just
                        GroupLeg
                            { groupLeg = These iL iR
                            , groupTime = These tL tR
                            }
                (Nothing, Just (tL, Just iL)) ->
                    Just
                        GroupLeg
                            { groupLeg = This iL
                            , groupTime = This tL
                            }
                (Just (_, Nothing), Just (tL, Just iL)) ->
                    Just
                        GroupLeg
                            { groupLeg = This iL
                            , groupTime = This tL
                            }
                (Just (tR, Just iR), Nothing) ->
                    Just
                        GroupLeg
                            { groupLeg = That iR
                            , groupTime = That tR
                            }
                (Just (tR, Just iR), Just (_, Nothing)) ->
                    Just
                        GroupLeg
                            { groupLeg = That iR
                            , groupTime = That tR
                            }

        in (g, mf{fixes = ysL})

    | ysL <- yssL
    , let zerothL =
            case ysL of
                [] ->
                    Nothing
                (zerothFixL : _) ->
                    let t = fixToUtc mark0 zerothFixL in
                    Just (t, Map.lookup (Just t) timeToLeg)

    | ysR <- yssR
    , let nthR =
            case reverse ysR of
                [] ->
                    Nothing
                (nthFixR : _) ->
                    let t = fixToUtc mark0 nthFixR in
                    Just (t, Map.lookup (Just t) timeToLeg)
    ]
    where
        FlyCut{uncut = mf@MarkedFixes{mark0, fixes}} = clipToFlown flyCut

        xs :: [Maybe ZoneTag]
        xs =
            tagZones tagInterp (zoneToCyl <$> zs)
            . unSelectedCrossings
            . selectedCrossings
            $ madeZones (spanner tagInterp) zoneToCyl task mf

        ts :: [Maybe UTCTime]
        ts = (fmap . fmap) (time . inter) xs

        timeToLeg :: Map.Map (Maybe UTCTime) LegIdx
        timeToLeg = Map.fromList $ zip ts (LegIdx <$> [1..])

        -- WARNING: Pilots can end up with duplicate timestamps when they are
        -- logging at a sub-second rate. For IGC files the HMS and ss fields are
        -- in the same B record but in different locations. If the parser does
        -- not know about the sub-second field then it will parse multiple fixes
        -- with the same HMS time.
        uniqueFixes = nub fixes

        yssL :: [[Kml.Fix]]
        yssL =
            split
                ( keepDelimsL
                $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                uniqueFixes

        yssR :: [[Kml.Fix]]
        yssR =
            split
                ( keepDelimsR
                $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                uniqueFixes

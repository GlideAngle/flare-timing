module Flight.Mask.Group (GroupLeg(..), GeoLeg(..)) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.These

import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Zone.Raw (Give)
import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Kml (MarkedFixes(..))
import Flight.Track.Time (LegIdx(..))
import Flight.Comp (Task(..), TimePass)
import Flight.Units ()
import Flight.Mask.Internal.Race ()
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Mask.Tag (GeoTag(..))

data GroupLeg =
    GroupLeg
        { groupLeg :: These LegIdx LegIdx
        , groupTime :: These UTCTime UTCTime
        }

class GeoTag g a => GeoLeg g a where
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
        :: (FlyClipping UTCTime MarkedFixes, Trig g a)
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> [TimePass]
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> [(Maybe GroupLeg, MarkedFixes)]

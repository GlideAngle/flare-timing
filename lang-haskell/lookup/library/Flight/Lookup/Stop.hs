module Flight.Lookup.Stop
    ( ScoredLookup(..)
    , stopFlying
    ) where

import Data.List (find)
import Control.Lens ((^?), element)
import Flight.Comp (IxTask(..), Pilot(..))
import Flight.Track.Stop (StopFraming(stopScored), TrackScoredSection(..))
import qualified Flight.Track.Stop as S (CompFraming(..))

type StopLookup a = IxTask -> Pilot -> Maybe a

newtype ScoredLookup =
    ScoredLookup (Maybe (StopLookup TrackScoredSection))

stopFlying :: Maybe S.CompFraming -> ScoredLookup
stopFlying = ScoredLookup . (fmap flyingTask)

flyingTask :: S.CompFraming -> IxTask -> Pilot -> Maybe TrackScoredSection
flyingTask S.CompFraming{stopFlying = xs} (IxTask i) pilot =
    case xs ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just ys -> stopScored . snd =<< find (\(p, _) -> p == pilot) ys

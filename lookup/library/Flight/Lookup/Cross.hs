module Flight.Lookup.Cross
    ( FlyingLookup(..)
    , crossFlying
    ) where

import Data.List (find)
import Control.Lens ((^?), element)
import Flight.Comp (IxTask(..), Pilot(..))
import Flight.Track.Cross (TrackFlyingSection(..))
import qualified Flight.Track.Cross as C (Crossing(..))

type CrossingLookup a = IxTask -> Pilot -> Maybe a

newtype FlyingLookup =
    FlyingLookup (Maybe (CrossingLookup TrackFlyingSection))

crossFlying :: Maybe C.Crossing -> FlyingLookup
crossFlying = FlyingLookup . (fmap flyingTask)

flyingTask :: C.Crossing -> IxTask -> Pilot -> Maybe TrackFlyingSection
flyingTask C.Crossing{flying = xs} (IxTask i) pilot =
    case xs ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just ys -> snd =<< find (\(p, _) -> p == pilot) ys

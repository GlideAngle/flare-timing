module Flight.Lookup.Cross
    ( FlyingLookup(..)
    , flyingTime
    ) where

import Data.List (find)
import Control.Lens ((^?), element)
import Flight.Comp (IxTask(..), Pilot(..))
import Flight.Track.Cross (TrackFlyingSection(..))
import qualified Flight.Track.Cross as C (CompFlying(..))

type CrossingLookup a = IxTask -> Pilot -> Maybe a

newtype FlyingLookup =
    FlyingLookup (Maybe (CrossingLookup TrackFlyingSection))

flyingTime :: Maybe C.CompFlying -> FlyingLookup
flyingTime = FlyingLookup . fmap flyingTask

flyingTask :: C.CompFlying -> IxTask -> Pilot -> Maybe TrackFlyingSection
flyingTask C.CompFlying{flying = xs} (IxTask i) pilot =
    case xs ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just ys -> snd =<< find (\(p, _) -> p == pilot) ys

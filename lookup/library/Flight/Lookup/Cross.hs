{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Lookup.Cross
    ( FlyingLookup(..)
    , crossFlying
    ) where

import Data.List (find)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Flight.Comp (IxTask(..), Pilot(..), FlyingSection)
import Flight.Track.Cross (TrackFlyingSection(..))
import qualified Flight.Track.Cross as C (Crossing(..))

type CrossingLookup a = IxTask -> Pilot -> Maybe a

newtype FlyingLookup = FlyingLookup (Maybe (CrossingLookup (FlyingSection Int)))

crossFlying :: Either String C.Crossing -> FlyingLookup
crossFlying (Left _) = FlyingLookup Nothing
crossFlying (Right x) = FlyingLookup (Just $ flyingTask x)

flyingTask :: C.Crossing -> IxTask -> Pilot -> Maybe (FlyingSection Int)
flyingTask C.Crossing{flying = xs} (IxTask i) pilot =
    case xs ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just ys ->
            join
            $ flyingPilot <$> find (\(p, _) -> p == pilot) ys

flyingPilot :: (Pilot, Maybe TrackFlyingSection) -> Maybe (FlyingSection Int)
flyingPilot (_, Nothing) = Nothing
flyingPilot (_, (Just TrackFlyingSection{flyingFixes})) = Just flyingFixes

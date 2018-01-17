{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Lookup.Cross
    ( FlyingLookup(..)
    , crossFlying
    , flyingRange
    ) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Flight.Comp (IxTask(..), Pilot(..), FlyingSection)
import Flight.Track.Cross (TrackFlyingSection(..))
import qualified Flight.Track.Cross as C (Crossing(..))

type CrossingLookup a = IxTask -> Pilot -> Maybe a

newtype FlyingLookup =
    FlyingLookup (Maybe (CrossingLookup TrackFlyingSection))

crossFlying :: Either String C.Crossing -> FlyingLookup
crossFlying = FlyingLookup . either (const Nothing) (Just . flyingTask)

flyingTask :: C.Crossing -> IxTask -> Pilot -> Maybe TrackFlyingSection
flyingTask C.Crossing{flying = xs} (IxTask i) pilot =
    case xs ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just ys ->
            join
            $ snd <$> find (\(p, _) -> p == pilot) ys

flyingRange :: FlyingLookup -> UTCTime -> IxTask -> Pilot -> FlyingSection UTCTime
flyingRange (FlyingLookup flying) mark0 iTask p =
    fromMaybe (Just (mark0, mark0))
    $ join (fmap flyingTimes . (\f -> f iTask p) <$> flying)

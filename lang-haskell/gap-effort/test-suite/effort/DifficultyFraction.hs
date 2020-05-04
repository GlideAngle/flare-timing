module DifficultyFraction (difficulty) where

import qualified "flight-gap-effort" Flight.Score as FS (gradeDifficulty)
import "flight-gap-allot" Flight.Score
    ( Pilot(..)
    , PilotName(..)
    , PilotId(..)
    , isNormal
    )
import "flight-gap-effort" Flight.Score
    ( DifficultyFraction(..)
    , ChunkDifficultyFraction(..)
    , Difficulty(..)
    )

import TestNewtypes

nullPilot :: Pilot
nullPilot = Pilot (PilotId "", PilotName "")

nullPilots :: [Pilot]
nullPilots = repeat nullPilot

difficulty :: DfTest -> Bool
difficulty (DfTest (dBest, xs)) =
    all isNormal $ f <$> diffFracs (FS.gradeDifficulty dBest nullPilots xs)
    where
        f (DifficultyFraction df) = df

diffFracs :: (a, Difficulty) -> [DifficultyFraction]
diffFracs = fmap frac . fractional . snd

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ViewPatterns #-}
{-# lANGUAGE ScopedTypeVariables #-}

{-|
Module      : Flight.Score
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides GAP scoring for hang gliding and paragliding competitons.
-}
module Flight.Score
    ( NominalLaunch
    , LaunchValidity
    , launchValidity
    ) where

import Data.Ratio ((%), numerator, denominator)

type NominalLaunch = Rational
type MinimumDist = Int
type NominalDist = Int
type NominalTime = Int
type NominalGoal = Int

type LaunchValidity = Rational
type TimeValidity = Rational
type DistanceValidity = Rational
type DayQuality = Rational

type DistancePoint = Rational
type SpeedPoint = Rational
type DeparturePoint = Rational
type ArrivalPoint = Rational

type Seconds = Int
type Metres = Int

data FixDistance = FixDistance Seconds Metres
data PointsAllocation =
    PointsAllocation { distance :: Rational
                     , speed :: Rational
                     , departure :: Rational
                     , arrival :: Rational
                     }

-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern num :% denom <- (\x -> (numerator x, denominator x) -> (num, denom))

launchValidity :: NominalLaunch -> Rational -> LaunchValidity
launchValidity (0 :% _) _ =
    1 % 1
launchValidity n (flying :% present) =
    toRational $ 0.027 * lvr + 2.917 * lvr * lvr + 1.944 * lvr * lvr * lvr
    where
        lvr' :: Double
        lvr' = fromIntegral flying / (fromIntegral present * fromRational n)

        lvr :: Double
        lvr = min lvr' 1

timeValidity :: NominalTime -> Int -> TimeValidity
timeValidity = undefined

distanceValidity :: NominalDist -> [Metres] -> DistanceValidity
distanceValidity = undefined

dayQuality :: LaunchValidity -> TimeValidity -> DistanceValidity -> DayQuality
dayQuality = undefined

distancePoints :: [Metres] -> [DistancePoint]
distancePoints = undefined

speedPoints :: [Seconds] -> [SpeedPoint]
speedPoints = undefined

departurePoints :: [FixDistance] -> [DeparturePoint]
departurePoints = undefined

arrivalPoints :: Int -> [ArrivalPoint]
arrivalPoints = undefined

allocatePoints :: Rational -> PointsAllocation
allocatePoints = undefined

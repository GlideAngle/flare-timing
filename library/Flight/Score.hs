{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Flight.Score
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides GAP scoring for hang gliding and paragliding competitons.
-}
module Flight.Score
    ( MinimumDist
    , NominalDist
    , NominalTime
    , NominalGoal
    , DayQuality
    , DistancePoint
    , SpeedPoint
    , DeparturePoint
    , ArrivalPoint
    , Seconds
    , Metres
    , FixDistance
    , PointsAllocation
    , distancePoints
    , speedPoints
    , departurePoints
    , arrivalPoints
    , allocatePoints
    ) where

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

launchValidity :: Rational -> LaunchValidity
launchValidity = undefined

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

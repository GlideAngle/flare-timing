{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Flight.Track.Point
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Task points.
-}
module Flight.Track.Point
    ( Pointing(..)
    , Allocation(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Score
    (GoalRatio, TaskPoints, Points, Validity, ValidityWorking, Weights)
import Flight.Pilot (Pilot)

-- | For each task, the points for that task.
data Pointing =
    Pointing 
        { validityWorking :: [Maybe ValidityWorking]
        , validity :: [Maybe Validity]
        , allocation :: [Maybe Allocation]
        , score :: [[(Pilot, (Points, TaskPoints))]]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Allocation =
    Allocation 
        { goalRatio :: GoalRatio
        , weight :: Weights
        , points :: Points
        , taskPoints :: TaskPoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering Pointing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- Pointing fields
        ("validityWorking", _) -> LT

        ("validity", "validityWorking") -> GT
        ("validity", _) -> LT

        ("allocation", _) -> GT

        -- Allocation fields
        ("goalRatio", _) -> LT

        ("weight", "goalRatio") -> GT
        ("weight", _) -> LT

        ("points", "goalRatio") -> GT
        ("points", "weight") -> GT
        ("points", _) -> LT

        ("taskPoints", "goalRatio") -> GT
        ("taskPoints", "weight") -> GT
        ("taskPoints", "points") -> GT
        ("taskPoints", _) -> LT

        ("score", _) -> GT

        -- DistanceValidityWorking fields
        ("sum", _) -> LT

        ("flying", "sum") -> GT
        ("flying", "nominalLaunch") -> GT
        ("flying", _) -> LT

        ("area", "sum") -> GT
        ("area", "flying") -> GT
        ("area", _) -> LT

        ("nominalGoal", "sum") -> GT
        ("nominalGoal", "flying") -> GT
        ("nominalGoal", "area") -> GT
        ("nominalGoal", _) -> LT

        ("nominalDistance", "sum") -> GT
        ("nominalDistance", "flying") -> GT
        ("nominalDistance", "area") -> GT
        ("nominalDistance", "nominalGoal") -> GT

        -- NOTE: Duplicate record fields, see TimeValidityWorking for wildcard.
        -- ("nominalDistance", _) -> LT

        ("minimumDistance", "sum") -> GT
        ("minimumDistance", "flying") -> GT
        ("minimumDistance", "area") -> GT
        ("minimumDistance", "nominalGoal") -> GT
        ("minimumDistance", "nominalDistance") -> GT
        ("minimumDistance", _) -> LT

        ("best", _) -> GT

        -- LaunchValidityWorking fields
        ("nominalLaunch", _) -> LT

        ("present", "flying") -> GT
        ("present", "nominalLaunch") -> GT
        ("present", _) -> LT

        -- TimeValidityWorking fields
        ("nominalTime", "_") -> LT

        ("bestTime", "nominalTime") -> GT
        ("bestTime", "_") -> LT

        ("nominalDistance", "nominalTime") -> GT
        ("nominalDistance", "bestTime") -> GT
        ("nominalDistance", _) -> LT

        ("bestDistance", _) -> GT

        -- Point fields
        ("reach", _) -> LT

        ("effort", "reach") -> GT
        ("effort", _) -> LT

        ("distance", "reach") -> GT
        ("distance", "effort") -> GT
        ("distance", _) -> LT

        ("leading", "reach") -> GT
        ("leading", "effort") -> GT
        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "reach") -> GT
        ("arrival", "effort") -> GT
        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", "reach") -> GT
        ("time", "effort") -> GT
        ("time", "distance") -> GT
        ("time", "leading") -> GT
        ("time", "arrival") -> GT
        ("time", _) -> LT

        -- Validity fields
        ("task", _) -> LT

        ("launch", "task") -> GT
        ("launch", _) -> LT

        -- NOTE: Duplicate field names between Point and Validity, redundant pattern.
        -- ("distance", "task") -> GT
        -- ("distance", "launch") -> GT
        -- ("distance", _) -> LT
        -- ("time", _) -> GT

        -- NOTE: Weight fields catered for by Point fields

        _ -> compare a b

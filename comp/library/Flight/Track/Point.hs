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
    , Weight(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Score
    (GoalRatio, DistanceWeight, LeadingWeight, ArrivalWeight, TimeWeight)

-- | For each task, the points for that task.
data Pointing =
    Pointing 
        { weight :: [Allocation]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Allocation =
    Allocation 
        { goalRatio :: GoalRatio
        , weight :: Weight
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Weight =
    Weight 
        { distance :: DistanceWeight
        , leading :: LeadingWeight
        , arrival :: ArrivalWeight
        , time :: TimeWeight
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering Pointing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("goalRatio", _) -> LT
        ("weight", _) -> GT

        ("distance", _) -> LT

        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", _) -> GT
        _ -> compare a b

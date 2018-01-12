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
    , Validity(..)
    , Allocation(..)
    , Weight(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Score
    (GoalRatio
    , DistanceWeight, LeadingWeight, ArrivalWeight, TimeWeight
    , LaunchValidity, DistanceValidity
    )

-- | For each task, the points for that task.
data Pointing =
    Pointing 
        { validity :: [Validity]
        , allocation :: [Allocation]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Validity =
    Validity 
        { launch :: LaunchValidity
        , distance :: DistanceValidity
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
        -- Allocation fields
        ("goalRatio", _) -> LT
        ("weight", _) -> GT

        -- Validity fields
        ("launch", _) -> LT
        ("distance ", "launch") -> GT

        -- Weight fields
        ("distance", _) -> LT

        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", _) -> GT

        -- Pointing fields
        ("validity", _) -> LT
        ("allocation", _) -> GT

        _ -> compare a b

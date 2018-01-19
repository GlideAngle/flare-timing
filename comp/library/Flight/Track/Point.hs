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
    ( GoalRatio
    , TaskPoints, Points, Validity, Weights
    )

-- | For each task, the points for that task.
data Pointing =
    Pointing 
        { validity :: [Maybe Validity]
        , allocation :: [Maybe Allocation]
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
        -- Allocation fields
        ("goalRatio", _) -> LT

        ("weight", "goalRatio") -> GT
        ("weight", _) -> LT

        ("points", "goalRatio") -> GT
        ("points", "weight") -> GT
        ("points", _) -> LT

        ("taskPoints", _) -> GT

        -- Validity fields
        ("task", _) -> LT

        ("launch", "task") -> GT
        ("launch", _) -> LT

        ("distance", "task") -> GT
        ("distance", "launch") -> GT
        ("distance", _) -> LT

        ("time", _) -> GT

        -- Weight fields
        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT


        -- Pointing fields
        ("validity", _) -> LT
        ("allocation", _) -> GT

        _ -> compare a b

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Land
    ( Landing(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Score (Lookahead(..))

-- | For each task, the masking for that task.
data Landing =
    Landing 
        { minDistance :: Double
        -- ^ The mimimum distance, set once for the comp. All pilots landing
        -- before this distance get this distance. The 100m segments start from
        -- here.
        , bestDistance :: [Maybe Double]
        -- ^ For each task, the best distance flown.
        , landout :: [Int]
        -- ^ For each task, the number of pilots landing out.
        , lookahead :: [Maybe Int]
        -- ^ For each task, the look ahead distance
        , lookaheadChunks :: [Maybe Lookahead]
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON Landing
instance FromJSON Landing

instance FieldOrdering Landing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("lookahead", _) -> LT

        _ -> compare a b

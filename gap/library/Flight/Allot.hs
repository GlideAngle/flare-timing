{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Allot
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , BestTime(..)
    , PilotTime(..)
    , bestTime
    , SpeedFraction(..)
    , speedFraction
    , BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , Lookahead(..)
    , Chunks(..)
    , ChunkedDistance(..)
    , lookahead
    , toChunk
    , chunks
    , DifficultyFraction(..)
    , difficultyFraction
    ) where

import Control.Newtype (Newtype(..))
import Data.Ratio ((%))
import Data.List (sort, group, minimum)
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure
    (One, (+:), (-:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showQuantity)
import Data.UnitsOfMeasure.Read (QuantityWithUnit(..), Some(..), readQuantity)

import Flight.Ratio (pattern (:%))
import Data.Aeson.ViaScientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Flight.Validity (MinimumDistance(..))
import Flight.Units ()

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | A 1-based rank of the pilot arrival at goal, 1st in is 1, 2nd is 2 etc.
newtype PositionAtEss = PositionAtEss Integer
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype ArrivalFraction = ArrivalFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces ArrivalFraction where
    defdp _ = DecimalPlaces 8

instance Newtype ArrivalFraction Rational where
    pack = ArrivalFraction
    unpack (ArrivalFraction a) = a

newtype SpeedFraction = SpeedFraction Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces SpeedFraction where
    defdp _ = DecimalPlaces 8

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

-- | Best time for the task, units of hours.
newtype BestTime = BestTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces BestTime where
    defdp _ = DecimalPlaces 8

instance Newtype BestTime Rational where
    pack = BestTime
    unpack (BestTime a) = a

-- | Pilot time for the task, units of hours.
newtype PilotTime = PilotTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces PilotTime where
    defdp _ = DecimalPlaces 8

instance Newtype PilotTime Rational where
    pack = PilotTime
    unpack (PilotTime a) = a

newtype BestDistance = BestDistance (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

newtype PilotDistance a = PilotDistance a deriving (Eq, Ord, Show)
newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

data ChunkedDistance =
    ChunkedDistance
        { mimimum :: MinimumDistance (Quantity Double [u| km |])
        , chunkOffset :: Int
        }
    deriving (Eq, Ord, Show)

newtype DifficultyFraction = DifficultyFraction Rational
    deriving (Eq, Ord, Show)

-- | How far to look ahead, in units of 100m chunks.
newtype Lookahead = Lookahead Int
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | A sequence of chunk ends, distances on course in km.
newtype Chunks = Chunks [Quantity Double [u| km |]]
    deriving (Eq, Ord, Show)

instance ToJSON Chunks where
    toJSON (Chunks xs) = toJSON $ showQuantity <$> xs

instance FromJSON Chunks where
    parseJSON o = do
        xs :: [String] <- parseJSON o
        let qs :: [Either _ _] = readQuantity <$> xs
        let ([], qs') = partitionEithers qs
        let qs'' =
                (\(Some (QuantityWithUnit (MkQuantity q) u)) -> q)
                <$> qs'
        return . Chunks $ qs''

deriving instance (ToJSON a, u ~ [u| km |]) => ToJSON (Quantity a u)
deriving instance (FromJSON a, u ~ [u| km |]) => FromJSON (Quantity a u)

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
    | n <= 0 =
        ArrivalFraction 0
    | rank <= 0 =
        ArrivalFraction 0
    | rank > n =
        ArrivalFraction 0
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
            ac = 1 - ((rank - 1) % n)

bestTime :: [PilotTime] -> Maybe BestTime
bestTime [] = Nothing
bestTime xs = let PilotTime t = minimum xs in Just $ BestTime t

speedFraction :: BestTime -> PilotTime -> SpeedFraction
speedFraction (BestTime best) (PilotTime t) =
    SpeedFraction $ max (0 % 1) sf
    where
        numerator = fromRational $ t - best :: Double
        denominator = fromRational best ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - toRational frac

-- | The linear fraction for distance.
linearFraction
    :: BestDistance
    -> PilotDistance (Quantity Double [u| km |]) 
    -> LinearFraction
linearFraction (BestDistance bd) (PilotDistance pd) =
    LinearFraction $ (np * db) % (dp * nb)
    where
        MkQuantity (nb :% db) = toRational' bd
        MkQuantity (np :% dp) = toRational' pd

-- | How many 100 m chunks to look ahead when working out the distance
-- difficulty.
lookahead
    :: BestDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Lookahead
lookahead (BestDistance (MkQuantity best)) xs =
    Lookahead . max 30 $
    if null xs then 0
               else round ((30 * best) / fromInteger pilotsLandedOut)
    where
        pilotsLandedOut = toInteger $ length xs

-- | A list of 100m chunks of distance starting from the minimum distance set
-- up for the competition. Pilots that fly less than minimum distance get
-- awarded that distance.
chunks :: MinimumDistance (Quantity Double [u| km |]) -> BestDistance -> Chunks
chunks (MinimumDistance md) (BestDistance best) =
    Chunks $ MkQuantity <$> [x0, x1 .. xN]
    where
        MkQuantity x0 = md
        MkQuantity x1 = md +: convert [u| 100m |]
        MkQuantity xN = best

-- | Converts from pilot distance to distance in units of the number of 100m
-- chunks offset from the minium distance set for the competition.
toChunk
    :: MinimumDistance (Quantity Double [u| km |])
    -> PilotDistance (Quantity Double [u| km |])
    -> ChunkedDistance
toChunk dMin@(MinimumDistance md) (PilotDistance d)
    | d <= md = ChunkedDistance dMin 0
    | otherwise = ChunkedDistance dMin $ round x
    where
        MkQuantity x = convert (d -: md) :: Quantity _ [u| hm |]

-- | For a list of distances flown by pilots, works out the distance difficulty
-- fraction for each pilot.
difficultyFraction
    :: MinimumDistance (Quantity Double [u| km |])
    -> BestDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [DifficultyFraction]
difficultyFraction md best xs =
    zipWith (diffByChunk diffScoreMap) xs' ys
    where
        n = lookahead best xs

        xs' = sort xs

        ys :: [ChunkedDistance]
        ys = toChunk md <$> xs'

        gs :: [[ChunkedDistance]]
        gs = group ys

        ns :: [(ChunkedDistance, Int)]
        ns = (\gXs@(gX : _) -> (gX, length gXs)) <$> gs

        nMap :: Map.Map ChunkedDistance Int
        nMap = Map.fromList ns

        lookaheadMap :: Map.Map ChunkedDistance Integer
        lookaheadMap = toInteger <$> difficulty md n nMap

        sumOfDiff :: Integer
        sumOfDiff = toInteger $ sum $ Map.elems lookaheadMap

        relativeDiffMap :: Map.Map ChunkedDistance Rational
        relativeDiffMap = (\d -> d % (2 * sumOfDiff)) <$> lookaheadMap

        -- TODO: If distance > best flown * 10 then diff score is 0.5.
        diffScoreMap :: Map.Map ChunkedDistance Rational
        diffScoreMap =
            snd $ Map.mapAccum (\acc x -> (acc + x, x)) 0 relativeDiffMap

diffByChunk
    :: Map.Map ChunkedDistance Rational
    -> PilotDistance (Quantity Double [u| km |])
    -> ChunkedDistance
    -> DifficultyFraction
diffByChunk
    diffMap
    (PilotDistance (MkQuantity pd))
    pc@(ChunkedDistance md pdChunk) =
    DifficultyFraction $
    pDiff
    + (pDiffNext - pDiff)
    * (((10 * n) % d) - (toInteger pdChunk % 1))
    where
        (n :% d) = toRational pd
        pDiff :: Rational
        pDiff = fromMaybe (0 % 1) $ Map.lookup pc diffMap

        (_, pDiffNext :: Rational) =
            fromMaybe (ChunkedDistance md 0, 0 % 1) $ Map.lookupGT pc diffMap

difficulty
    :: MinimumDistance (Quantity Double [u| km |])
    -> Lookahead
    -> Map.Map ChunkedDistance Int
    -> Map.Map ChunkedDistance Int
difficulty _ (Lookahead n) nMap =
    Map.mapWithKey f nMap
    where
        f :: ChunkedDistance -> Int -> Int
        f (ChunkedDistance _ cd) _ =
            sum $ Map.elems filtered
            where
                kMin = cd
                kMax = cd + n

                filtered =
                    Map.filterWithKey
                        (\(ChunkedDistance _ k) _ -> kMin <= k && k <= kMax)
                        nMap

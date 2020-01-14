{-|
Module      : Flight.Track.Point
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Task placing.
-}
module Flight.Track.Place
    ( rankByTotal
    , rankByArrival
    , reIndex
    ) where

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Time.Clock (UTCTime)

import Flight.Track.Point (Breakdown(..))
import Flight.Score (Pilot(..), TaskPoints(..), TaskPlacing(..), ArrivalPlacing(..))
import Data.Ratio.Rounding (dpRound)

-- | Reindexing produces 1,2=,2=,4 and not 1,2=,2=,3 or 1,3=,3=,4.
-- >>> reIndex []
-- []
--
-- 1=,1=,3,4,5
-- >>> reIndex [(1,[1,1]),(2,[2]),(3,[3]),(4,[4])]
-- [(1,[1,1]),(3,[2]),(4,[3]),(5,[4])]
--
-- 1,2=,2=,2=,2=,2=,2=,8=,8=,10
-- >>> reIndex [(1,[1]),(2,[2,2,2,2,2,2]),(3,[3,3]),(4,[4])]
-- [(1,[1]),(2,[2,2,2,2,2,2]),(8,[3,3]),(10,[4])]
--
-- 1,2=,2=,4
-- >>> reIndex [(1,[1]),(3,[2,2]),(4,[3])]
-- [(1,[1]),(2,[2,2]),(4,[3])]
--
-- 1,2=,2=,4
-- >>> reIndex [(3,[1]),(2,[2,2]),(1,[3])]
-- [(1,[1]),(2,[2,2]),(4,[3])]
reIndex :: [(Integer, [a])] -> [(Integer, [a])]
reIndex xs =
    zipWith3
        (\i zs o ->
            -- NOTE: Use j so that we get; 1,2=,2=,4 and not 1,3=,3=,4.
            let j = fromIntegral $ length zs - 1
            in (i + (fromIntegral o) - j, zs))
        [1..]
        ys
        offsets
    where
        (_, ys) = unzip xs
        lens = (\y -> (length y) - 1) <$> ys
        offsets = scanl1 (+) lens

-- SEE: https://stackoverflow.com/questions/51572782/how-to-create-a-ranking-based-on-a-list-of-scores-in-haskell
-- SEE: https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
rankByTotal :: [(Pilot, Breakdown)] -> [(Pilot, Breakdown)]
rankByTotal xs =
    [ (rankScore f ii) <$> y
    | (ii, ys) <-
                reIndex
                . zip [1..]
                . groupBy ((==) `on` truncateTaskPoints . total . snd)
                $ xs
    , let f = if length ys == 1 then TaskPlacing else TaskPlacingEqual
    , y <- ys
    ]

truncateTaskPoints :: TaskPoints -> Integer
truncateTaskPoints (TaskPoints x) = truncate . dpRound 0 $ x

rankScore :: (Integer -> TaskPlacing) -> Integer -> Breakdown -> Breakdown
rankScore f ii b = b{place = f ii}

rankByArrival :: [UTCTime] -> [(UTCTime, ArrivalPlacing)]
rankByArrival ts =
    [ (y, f ii)
    | (ii, ys) <-
                reIndex
                . zip [1..]
                . groupBy (==)
                $ sort ts
    , let n = length ys
    , let f =
              if n == 1
                 then ArrivalPlacing
                 else flip ArrivalPlacingEqual $ fromIntegral n
    , y <- ys
    ]

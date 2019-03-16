module Flight.Track.Range (asRanges, asRollovers, asRolloversBy) where

import Data.List (findIndex)
import Data.List.Split (chop)

-- | Compresses sequences of consecutive values as ranges.
-- >>> asRanges []
-- []
-- >>> asRanges [1]
-- [(1,1)]
-- >>> asRanges [1,2]
-- [(1,2)]
-- >>> asRanges [0..9]
-- [(0,9)]
-- >>> asRanges [1,3,4,7,8,9]
-- [(1,1),(3,4),(7,9)]
asRanges :: (Ord a, Num a) => [a] -> [(a, a)]
asRanges [] = []
asRanges [x, y] = [(x, y)]
asRanges zs =
    chop
        (\xs@(x0 : _) ->
            let xys = zip xs $ x0 : xs in
            case findIndex (\(x, y) -> y + 1 < x) xys of
                Nothing ->
                    ( (x0, let (xN : _) = reverse xs in xN)
                    , []
                    )
                Just i ->
                    ((x0, let (xN : _) = reverse (take i xs) in xN)
                    , drop i xs
                    ))
        zs

-- | Splits the list every time there's a decrement.
-- >>> asRollovers []
-- []
-- >>> asRollovers [1]
-- [[1]]
-- >>> asRollovers [1,2]
-- [[1,2]]
-- >>> asRollovers [0..9]
-- [[0,1,2,3,4,5,6,7,8,9]]
-- >>> asRollovers [7,8,9,1,2,3]
-- [[7,8,9],[1,2,3]]
-- >>> asRollovers [7,8,9,1,2,3,3,2,1]
-- [[7,8,9],[1,2,3,3],[2],[1]]
-- >>> asRollovers [7,8,9,1,2,3,3,2,1,1,2,3,7,8,9]
-- [[7,8,9],[1,2,3,3],[2],[1,1,2,3,7,8,9]]
asRollovers :: Ord a => [a] -> [[a]]
asRollovers [] = []
asRollovers [x, y] = if x <= y then [[x, y]] else [[x],[y]]
asRollovers zs =
    chop
        (\xs@(x0 : _) ->
            let xys = zip (x0 : xs) xs in
            case findIndex (\(x, y) -> y < x) xys of
                Nothing -> (xs, [])
                Just i -> (take i xs, drop i xs))
        zs

asRolloversBy :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
asRolloversBy _ [] = []
asRolloversBy p [x, y] = if x `p` y /= GT then [[x, y]] else [[x],[y]]
asRolloversBy p zs =
    chop
        (\xs@(x0 : _) ->
            let xys = zip (x0 : xs) xs in
            case findIndex (\(x, y) -> y `p` x == LT) xys of
                Nothing -> (xs, [])
                Just i -> (take i xs, drop i xs))
        zs

module Flight.Track.Range
    ( asRanges
    , asRollovers
    , asRolloversBy
    , deleteSort
    ) where

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
        (\case
            [] -> let z = fromIntegral (0 :: Int) in ((z, z), [])
            xs@(x0 : _) ->
                let xys = zip xs $ x0 : xs in
                case findIndex (\(x, y) -> y + 1 < x) xys of
                    Nothing ->
                        case reverse xs of
                            [] -> ((x0, x0), [])
                            (xN : _) -> ((x0, xN), [])
                    Just i ->
                        case reverse (take i xs) of
                            [] -> ((x0, x0), drop i xs)
                            (xN : _) -> ((x0, xN), drop i xs))
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
        (\case
            [] -> ([], [])
            xs@(x0 : _) ->
                let xys = zip (x0 : xs) xs in
                case findIndex (uncurry (>)) xys of
                    Nothing -> (xs, [])
                    Just i -> (take i xs, drop i xs))
        zs

-- | Splits the list every time there's a decrement.
-- prop> \xs -> asRolloversBy compare xs == asRollovers xs
-- True
--
-- >>> asRolloversBy (\a b -> compare a (b + 1)) [1]
-- [[1]]
-- >>> asRolloversBy (\a b -> compare a (b + 1)) [2,1]
-- [[2,1]]
-- >>> asRolloversBy (\a b -> compare a (b + 1)) [3,1]
-- [[3],[1]]
asRolloversBy :: (a -> a -> Ordering) -> [a] -> [[a]]
asRolloversBy _ [] = []
asRolloversBy p [x, y] = if x `p` y /= GT then [[x, y]] else [[x],[y]]
asRolloversBy p zs =
    chop
        (\case
            [] -> ([], [])
            xs@(x0 : _) ->
                let xys = zip (x0 : xs) xs in
                case findIndex (\(x, y) -> x `p` y == GT) xys of
                    Nothing -> (xs, [])
                    Just i -> (take i xs, drop i xs))
        zs
-- |
-- prop> \xs -> length (deleteSort xs) <= length xs
-- True
--
-- >>> permutations [1..3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
--
-- >>> deleteSort <$> permutations [1,2,3]
-- [[1,2,3],[2,3],[3],[2,3],[3],[1,3]]
deleteSort :: Ord a => [a] -> [a]
deleteSort [] = []
deleteSort [x] = [x]
deleteSort xs@(x0 : _) =
    let ys = fmap snd $ filter (uncurry (<=)) $ zip (x0 : xs) xs
    in if ys /= xs then deleteSort ys else xs

-- $setup
-- >>> import Data.List (permutations)

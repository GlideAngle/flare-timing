module Flight.Track.Range (asRanges) where

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
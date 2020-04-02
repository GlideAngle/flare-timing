{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Zone.SpeedSection (SpeedSection, sliceZones, restartZones) where

-- | A 1-based index into the list of control zones marking the speed section.
type SpeedSection = Maybe (Int, Int)

-- | Slice the speed section from a list.
-- prop> \x -> sliceZones x [] == []
-- prop> \xs -> sliceZones Nothing xs == xs
-- prop> \xs -> sliceZones (Just (1, length xs)) xs == xs
-- prop> \xs -> sliceZones (Just (2, length xs)) xs == drop 1 xs
-- prop> \i xs -> sliceZones (Just (i, length xs)) xs == drop (i - 1) xs
-- prop> \i j xs -> i < j ==> sliceZones (Just (i, j)) xs == take (j - i + 1) (drop (i - 1) xs)
sliceZones :: SpeedSection -> [a] -> [a]
sliceZones = \case
    Nothing -> id
    Just (s', e') -> let (s, e) = (s' - 1, e' - 1) in take (e - s + 1) . drop s

-- | Replace the start element with another.
-- prop> \s x -> restartZones s x [] == []
-- prop> \x xs -> restartZones Nothing x xs == xs
-- prop> \x xs -> not (null xs) ==> restartZones (Just (1, length xs)) x xs == x : drop 1 xs
-- prop> \x xs -> length xs >= 2 ==> restartZones (Just (2, length xs)) x xs == take 1 xs ++ (x : drop 2 xs)
-- >>> restartZones (Just (1, 1)) 0 [1..4]
-- [0,2,3,4]
-- >>> restartZones (Just (1, 2)) 0 [1..4]
-- [0,2,3,4]
-- >>> restartZones (Just (1, 3)) 0 [1..4]
-- [0,2,3,4]
-- >>> restartZones (Just (1, 4)) 0 [1..4]
-- [0,2,3,4]
-- >>> restartZones (Just (2, 2)) 0 [1..4]
-- [1,0,3,4]
-- >>> restartZones (Just (2, 3)) 0 [1..4]
-- [1,0,3,4]
-- >>> restartZones (Just (2, 4)) 0 [1..4]
-- [1,0,3,4]
-- >>> restartZones (Just (3, 3)) 0 [1..4]
-- [1,2,0,4]
-- >>> restartZones (Just (3, 4)) 0 [1..4]
-- [1,2,0,4]
-- >>> restartZones (Just (4, 4)) 0 [1..4]
-- [1,2,3,0]
restartZones :: SpeedSection -> a -> [a] -> [a]
restartZones ss x xs
    | null ss = xs
    | null xs = []
    | otherwise =
        case ss of
            Nothing -> xs
            Just (i, _) | i < 1 -> xs
            Just (i, _) ->
                let i' = i - 1
                    (_ : ys) = drop i' xs
                in take i' xs ++ (x : ys)

-- $setup
-- >>> import Test.QuickCheck

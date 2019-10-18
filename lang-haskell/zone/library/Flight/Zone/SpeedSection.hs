{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Zone.SpeedSection (SpeedSection, sliceZones) where

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

-- $setup
-- >>> import Test.QuickCheck
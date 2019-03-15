module Flight.Igc.Fix
    ( igcEqOrEqOnTime
    , igcBumpOver
    ) where

import Flight.Igc.Record
import Flight.Track.Range (asRollovers)

igcEqOrEqOnTime :: IgcRecord -> IgcRecord -> Bool
igcEqOrEqOnTime (B t0 _ _ _ _) (B t1 _ _ _ _) = t0 == t1
igcEqOrEqOnTime a b = a == b

-- | The B record only records time of day. If the sequence is not increasing
-- then for every rollback add a bump 24 hrs.
--
-- >>> asRollovers [7,9,2,3]
-- [[7,9],[2,3]]
igcBumpOver :: [IgcRecord] -> [IgcRecord]
igcBumpOver xs =
    bumpOver
        (flip $ addHoursIgc . Hour . show)
        [0 :: Integer, 24..]
        xs

-- | Apply a bump from a list every time there is a roll over.
--
-- >>> bumpOver (+) [0,10..] [7,9,2,3,1]
-- [7,9,12,13,21]
bumpOver :: Ord a => (a -> b -> a) -> [b] -> [a] -> [a]
bumpOver add ns xs =
    concat
    [ (`add` n) <$> ys
    | ys <- asRollovers xs
    | n <- ns
    ]


module Flight.Mask.Tag.OrdLists (trimOrdLists) where

import Data.List (findIndex)

import Flight.Units ()
import Flight.Mask.Internal.Race ()

-- | If I have three sorted lists xs, ys and zs, discard elements of ys that
-- are greater than the last element of zs, then again filter ys so that each
-- element is greater than the first element of xs.
--
-- The reason for doing the comparison between ys and zs first is that on
-- triangle courses,  the first zone is commonly not part of the speed section.
-- This zone may only have crossings made after goal, at the end of the day's
-- racing. These will have high indices and I want to discard them early on in
-- the trimming.
--
-- It is alright too to end up with a null first list of crossings. This will
-- happen in an aerotow comp when the pilot is towed up from outside the first
-- zone.
trimToOrder :: Ord a => [a] -> [a] -> [a] -> [a]

trimToOrder _ [] _ = []

trimToOrder [] ys (zMin : _) = filter (< zMin) ys

trimToOrder xs ys zs@(_ : _) =
    case xs' of
        [] -> ys'
        (x : _) -> filter (> x) ys'
    where
        (xs', ys') =
            case reverse zs of
                [] -> ([], [])
                (zMax : _) ->
                    case (findIndex (< zMax) xs, findIndex (< zMax) ys) of
                        (_, Nothing) -> (xs, ys)
                        (Nothing, _) -> ([], filter (< zMax) ys)
                        (Just _, Just _) -> (filter (< zMax) xs, filter (< zMax) ys)

trimToOrder (xMin : _) ys [] = filter (> xMin) ys

trimToOrder [] ys [] = ys

-- | Removes elements of the list of lists so that each list only has elements
-- less than elements of subsequent lists and greater than previous lists. Once
-- the list is trimmed, trimming again will not change it.
--
-- >>> trimOrdLists [[16],[757,964,1237,1251,1369,7058],[2622,2662],[5324,5329],[23,86,89,91,97,98,103,108,109]]
-- [[16],[757,964,1237,1251,1369],[2622,2662],[5324,5329],[]]
--
-- >>> trimOrdLists [[16],[757,964,1237,1251,1369],[2622,2662],[5324,5329],[]]
-- [[16],[757,964,1237,1251,1369],[2622,2662],[5324,5329],[]]
--
-- >>> trimOrdLists [[6514,6519],[753,6254],[3106,3953],[],[6502,6529,6602,6616,6757]]
-- [[],[753],[3106,3953],[],[]]
--
-- >>> trimOrdLists [[],[753],[3106,3953],[],[]]
-- [[],[753],[3106,3953],[],[]]
--
-- >>> trimOrdLists [[25,4953,4955],[783,809,811,817,820,822,952,4812],[1810,1816],[3778,3781],[30,66,144,145,149,151,153,4950,4960,4965]]
-- [[25],[783,809,811,817,820,822,952],[1810,1816],[3778,3781],[4950,4960,4965]]
--
-- >>> trimOrdLists [[25],[783,809,811,817,820,822,952],[1810,1816],[3778,3781],[4950,4960,4965]]
-- [[25],[783,809,811,817,820,822,952],[1810,1816],[3778,3781],[4950,4960,4965]]
--
-- >>> trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- >>> trimOrdLists [[294],[1367],[2207,2209],[3914,3920],[4711]]
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- In another example a pilot only makes it around some of the course but
-- had flown through the goal cylinder before starting the speed section.
--
-- >>> trimOrdLists [[29],[276],[],[],[32,67,68,69,78]]
-- [[29],[276],[],[],[]]
--
-- >>> trimOrdLists [[29],[276],[],[],[]]
-- [[29],[276],[],[],[]]
--
-- >>> trimOrdLists [[4588,4592],[],[1714,1720],[3539,3546],[4584]]
-- [[4588,4592],[],[],[],[]]
--
-- >>> trimOrdLists [[4588,4592],[],[],[],[]]
-- [[4588,4592],[],[],[],[]]
--
-- >>> trimOrdLists [[4588,4592],[30,578,583,721,4400],[1714,1720],[3539,3546],[4584]]
-- [[],[30,578,583,721],[1714,1720],[3539,3546],[4584]]
--
-- >>> trimOrdLists [[],[30,578,583,721],[1714,1720],[3539,3546],[4584]]
-- [[],[30,578,583,721],[1714,1720],[3539,3546],[4584]]
--
-- >>> trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- >>> trimOrdLists [[294],[1367],[2207,2209],[3914,3920],[4711]]
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- >>> trimOrdLists [[80],[3190],[3582,3966],[5179,5190],[7383,7401],[9908,9936],[3582,3966,7246,7574],[8978,9627],[]]
-- [[80],[3190],[3582,3966],[5179,5190],[7383,7401],[9908,9936],[],[],[]]
--
-- >>> trimOrdLists [[80],[3190],[3582,3966],[5179,5190],[7383,7401],[9908,9936],[],[],[]]
-- [[80],[3190],[3582,3966],[5179,5190],[7383,7401],[9908,9936],[],[],[]]
trimOrdLists :: Ord a => [[a]] -> [[a]]
trimOrdLists ys =
    if ys == ys' then nonNullBlock ys else trimOrdLists ys'
    where
        xs = [] : ys
        zs = drop 1 ys ++ [[]]
        ys' = zipWith3 trimToOrder xs ys zs

-- | Prepends a number of empty lists.
prependNull :: Int -> [[a]] -> [[a]]
prependNull n xs =
    replicate n [] ++ xs

-- | Going left to right, as soon as an empty list is encountered, all
-- subsequent lists are made null too.
lockNullRight :: [[a]] -> [[a]]
lockNullRight xs =
    take (length xs) $ takeWhile (not . null) xs ++ repeat []

-- | Nulls are kept on the left, then a sequence of non-null lists. On the
-- first occurence of a null list, all further lists are replaced by null lists
-- on the right.
nonNullBlock :: [[a]] -> [[a]]
nonNullBlock xs =
    prependNull (length xs - length ys) ys
    where
        ys = lockNullRight $ dropWhile null xs

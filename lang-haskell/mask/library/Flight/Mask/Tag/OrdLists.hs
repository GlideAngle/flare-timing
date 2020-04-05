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
    case (xs, xs') of
        (_, []) -> []
        ([], _) -> ys'
        (x : _, x' : _) -> filter (> x) $ filter (> x') ys'
    where
        (xs', ys') =
            case reverse zs of
                [] -> ([], [])
                (zMax : _) ->
                    case (findIndex (< zMax) xs, findIndex (< zMax) ys) of
                        (Nothing, Nothing) -> (xs, ys)
                        (_, Nothing) -> (filter (< zMax) xs, ys)
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
--
-- PWC2019-1 task 1 has turnpoints D02087 ->> B15078-B15078-B34204-A05034-B21248-B15078-D01128 ->> PC
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Deonir SPANCERSKI"
-- >>> trimOrdLists [[80,5266,5355],[3190],[3582,3966,7246,7574],[5179,5190],[7383,7401],[9908,9936],[3582,3966,7246,7574],[8978,9627],[]]
-- [[80],[3190],[3582,3966],[5179,5190],[7383,7401],[9908,9936],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Sebastian OSPINA"
-- >>> trimOrdLists [[81,237,244,247,291,4845,4902,5125,5137],[3168],[3590,4068,7976,8183],[4980,4991],[8003,8015],[9949,9959],[3590,4068,7976,8183],[9130,9669],[]]
-- [[81,237,244,247,291],[3168],[3590,4068],[4980,4991],[8003,8015],[9949,9959],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Chigwon WON"
-- >>> trimOrdLists [[72,5690,5755,6931,6935,6950,6962,7221,7228,7243,7254,7270,7271,7289,7297],[3615],[4047,4520,7776,8115],[5582,5589],[7899,7921],[10476,10489],[4047,4520,7776,8115],[9585,9604,9627,9677,9702,10195],[]]
-- [[72],[3615],[4047,4520],[5582,5589],[7899,7921],[10476,10489],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Jose REBELO"
-- >>> trimOrdLists [[47,4281,4418],[6016],[2764,3149,6302,6712,11169,11177],[5524,5535],[6402,6408],[8389,8396],[2764,3149,6302,6712,11169,11177],[7634,8178,8662,10331],[]]
-- [[47,4281,4418],[6016],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Yoshiki KUREMOTO"
-- >>> trimOrdLists [[75,76,77,269,276,278,339,5984,6060],[7326],[4272,4708,7690,7807,7826,8289],[5905,5907],[7743,7912],[10247,10260],[4272,4708,7690,7807,7826,8289],[9160,10058,10508,10951],[]]
-- [[75,76,77,269,276,278,339,5984,6060],[7326],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Adrian SEITZ"
-- >>> trimOrdLists [[74,236,284],[7315],[4224,4827,7686,8213],[6348,6357,6364,6377],[7766,7786,7802,7811,7826,7837,7849,7894],[10084,10092],[4224,4827,7686,8213],[8868,8885,8888,9892,10334,10766],[]]
-- [[74,236,284],[7315],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Thomas GURY"
-- >>> trimOrdLists [[76,6743,6746,6760,6768,6781,6794,6977,6981,6999,7010,7025,7031],[7117],[3813,4413,7634,8231],[6277,6308],[3794,3838,7815,7822],[9840,9853],[3813,4413,7634,8231],[9150,9668,10083,10485],[]]
-- [[76,6743,6746,6760,6768,6781,6794,6977,6981,6999,7010,7025,7031],[7117],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Marko NOVAK"
-- >>> trimOrdLists [[60,4631,4720],[5206],[3027,3379,6303,6506],[4542,4558],[6071,6354],[8234,8255],[3027,3379,6303,6506],[7582,8028,8455,8893,9048,9336],[]]
-- [[60,4631,4720],[5206],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Constance METTETAL"
-- >>> trimOrdLists [[59],[6596],[3892,4431,6954,7411],[6214,6223],[7095,7117],[9190,9203],[3892,4431,6954,7411],[8345,8934,9468,9964],[]]
-- [[59],[6596],[],[],[],[],[],[],[]]
--
-- cross-zone --file=PWC2019-1 --task=1 "--pilot=Owen SHOEMAKER"
-- >>> trimOrdLists [[62,292,312,338,364],[674,1756],[],[],[],[],[],[],[]]
-- [[62,292,312,338,364],[674,1756],[],[],[],[],[],[],[]]
trimOrdLists :: Ord a => [[a]] -> [[a]]
trimOrdLists (xs : ys@(y : _) : zs) = _trim (filter (< y) xs : ys : zs)
trimOrdLists xss = _trim xss

_trim :: Ord a => [[a]] -> [[a]]
_trim ys =
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

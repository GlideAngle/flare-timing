{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ViewPatterns #-}
{-# lANGUAGE TypeSynonymInstances #-}
{-# lANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Flight.EdgeToEdge
    ( Samples(..)
    , circumSample
    , distanceEdgeToEdge
    ) where

import Data.Ratio (Ratio, (%), numerator, denominator)
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree) 
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
-- import Data.Graph.Inductive.Graph (Graph(..), Node, Path, mkGraph, match)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps)
import Flight.Zone (Zone(..), Radius(..))
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

distanceEdgeToEdge :: [Zone] -> (TaskDistance, [LatLng])
distanceEdgeToEdge [] = (TaskDistance 0, [])
distanceEdgeToEdge [_] = (TaskDistance 0, [])
distanceEdgeToEdge xs =
    (dist, ys)
    where
        gr :: Gr LatLng TaskDistance
        gr = buildGraph xs

        startNode = 0
        endNode = 1

        spt :: LRTree TaskDistance
        spt = spTree startNode gr

        dist :: TaskDistance
        dist = getDistance endNode spt

        ps :: Path
        ps = getLPathNodes endNode spt

        ys :: [LatLng]
        ys =
            catMaybes $
            (\p ->
                case match p gr of
                     (Nothing, _) -> Nothing
                     (Just (_, _, latlng, _), _) -> Just latlng
            )
            <$> ps

buildGraph :: [Zone] -> Gr LatLng TaskDistance
buildGraph zones =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[LatLng]]
        nodes' = sample (Samples 10) <$> zones

        len :: Int
        len = sum $ map length nodes'

        iiNodes :: [[(Node, LatLng)]]
        iiNodes = zip [1 .. ] <$> nodes'

        iNodes :: [[(Node, LatLng)]]
        iNodes = zipWith (\i xs -> first (\x -> x + i * len) <$> xs) [1 .. ] iiNodes

        edges' :: [[LEdge TaskDistance]]
        edges' = zipWith g iNodes (tail iNodes)

        flatEdges :: [LEdge TaskDistance]
        flatEdges = concat edges'

        flatNodes :: [(Node, LatLng)]
        flatNodes = concat iNodes

        f :: (Node, LatLng) -> (Node, LatLng) -> LEdge TaskDistance
        f (i, x) (j, y) = (i, j, distancePointToPoint [Point x, Point y])

        g :: [(Node, LatLng)] -> [(Node, LatLng)] -> [LEdge TaskDistance]
        g xs ys = [ f x y | x <- xs, y <- ys]

newtype Samples = Samples Integer deriving (Eq, Ord, Show)

sample :: Samples -> Zone -> [LatLng]
sample _ (Point x) = [x]
sample _ (Vector _ x) = [x]
sample n (Cylinder r x) = fst $ circumSample n r x
sample n (Conical _ r x) = fst $ circumSample n r x
sample n (Line r x) = fst $ circumSample n r x
sample n (SemiCircle r x) = fst $ circumSample n r x
 
-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern (:%) :: forall t. t -> t -> Ratio t
pattern num :% denom <- (\x -> (numerator x, denominator x) -> (num, denom))

circum :: Epsilon -> Radius -> LatLng -> Rational -> LatLng
circum _ (Radius rRadius) (LatLng (rlat, rlng)) rtc =
    LatLng (toRational lat', toRational lng')
    where
        lat :: Double
        lat = fromRational rlat

        lng :: Double
        lng = fromRational rlng

        tc :: Double
        tc = fromRational rtc

        radius :: Double
        radius = fromRational rRadius

        bigR = fromRational earthRadius

        lat' = asin (sin lat * cos d + cos lat * sin d * cos tc)

        dlng = atan ((sin tc * sin d * cos lat) / (cos d - sin lat * sin lat))

        a = lng - dlng + pi 
        b = 2 * pi 
        lng' = mod' a b - pi

        d = radius / bigR

-- | SEE: http://www.edwilliams.org/avform.htm#LL
circumSample :: Samples -> Radius -> LatLng -> ([LatLng], [Double])
circumSample (Samples samples) radius center =
    unzip zs
    where
        (Epsilon eps) = defEps

        xs :: [Rational]
        xs = [ ((2 * n) % samples) * F.pi eps | n <- [0 .. samples] ]

        ys = circum defEps radius center <$> xs
        zs = (\x -> (x, f $ distancePointToPoint [Point center, Point x])) <$> ys

        f :: TaskDistance -> Double
        f (TaskDistance d) = fromRational d

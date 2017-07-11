module Flight.EdgeToEdge
    ( Samples(..)
    , Tolerance(..)
    , EdgeDistance(..)
    , circumSample
    , distanceEdgeToEdge
    , buildGraph
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree) 
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
import Data.Graph.Inductive.Graph (Graph(..), Node, Path, LEdge, mkGraph, match)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps, degToRad, radToDeg)
import Flight.Zone (Zone(..), Radius(..), center)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.Separated (separatedZones)

newtype TrueCourse = TrueCourse Rational deriving (Eq, Ord, Show)

data EdgeDistance =
    EdgeDistance
        { centers :: TaskDistance
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone.
        , edges :: TaskDistance
        -- ^ The distance from the edge of the first zone to the edge of
        -- the last zone.
        , centerLine :: [LatLng]
        -- ^ The points of the 'centers' distance.
        , edgeLine :: [LatLng]
        -- ^ The points of the 'edges' distance.
        }

zero :: EdgeDistance
zero =
    EdgeDistance { centers = TaskDistance 0
                 , edges = TaskDistance 0
                 , centerLine = []
                 , edgeLine = []
                 }

distanceEdgeToEdge :: Samples -> Tolerance -> [Zone] -> EdgeDistance
distanceEdgeToEdge _ _ [] = zero
distanceEdgeToEdge _ _ [_] = zero
distanceEdgeToEdge samples tolerance xs =
    case xs of
        [] ->
            zero

        [_] ->
            zero

        [_, _] ->
            EdgeDistance { centers = d
                         , edges = d
                         , centerLine = ptsCenterLine
                         , edgeLine = ptsCenterLine
                         }

        (_ : _) ->
            EdgeDistance { centers = d
                         , edges = d'
                         , centerLine = ptsCenterLine
                         , edgeLine = ptsEdgeLine
                         }

            where
                ptsEdgeLine =
                    drop 1
                    $ reverse
                    $ drop 1
                    $ reverse ptsCenterLine

                d' = distancePointToPoint (Point <$> ptsEdgeLine)

    where
        (d, ptsCenterLine) = distance samples tolerance xs

distance :: Samples -> Tolerance -> [Zone] -> (TaskDistance, [LatLng])
distance _ _ [] = (TaskDistance 0, [])
distance _ _ [_] = (TaskDistance 0, [])
distance samples tolerance xs
    | not $ separatedZones xs = (TaskDistance 0, [])
    | length xs < 3 = (pointwise, centers')
    | otherwise =
        case dist of
             Nothing -> (pointwise, centers')
             Just y ->
                if y >= pointwise
                   then (pointwise, centers')
                   else (y, ys)
        where
            pointwise = distancePointToPoint xs
            centers' = center <$> xs

            gr :: Gr LatLng TaskDistance
            gr = buildGraph samples tolerance xs

            (startNode, endNode) = nodeRange gr

            spt :: LRTree TaskDistance
            spt = spTree startNode gr

            dist :: Maybe TaskDistance
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

buildGraph :: Samples -> Tolerance -> [Zone] -> Gr LatLng TaskDistance
buildGraph samples tolerance zones =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[LatLng]]
        nodes' = sample samples tolerance <$> zones

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

        -- | NOTE: The shortest path may traverse a cylinder so I include
        -- edges within a cylinder as well as edges to the next cylinder.
        g :: [(Node, LatLng)] -> [(Node, LatLng)] -> [LEdge TaskDistance]
        g xs ys =
            [ f x1 x2 | x1 <- xs, x2 <- xs ] ++ [ f x y | x <- xs, y <- ys]

newtype Samples = Samples Integer deriving (Eq, Ord, Show)
newtype Tolerance = Tolerance Rational deriving (Eq, Ord, Show)

sample :: Samples -> Tolerance -> Zone -> [LatLng]
sample _ _ (Point x) = [x]
sample _ _ (Vector _ x) = [x]
sample n t (Cylinder r x) = fst $ circumSample n t r x
sample n t (Conical _ r x) = fst $ circumSample n t r x
sample n t (Line r x) = fst $ circumSample n t r x
sample n t (SemiCircle r x) = fst $ circumSample n t r x
 
circum :: LatLng -> Epsilon -> Radius -> TrueCourse -> LatLng
circum (LatLng (latDegree, lngDegree)) _ (Radius rRadius) (TrueCourse rtc) =
    LatLng (radToDeg defEps $ toRational lat', radToDeg defEps $ toRational lng')
    where
        lat :: Double
        lat = fromRational $ degToRad defEps latDegree

        lng :: Double
        lng = fromRational $ degToRad defEps lngDegree

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
circumSample :: Samples -> Tolerance -> Radius -> LatLng -> ([LatLng], [Double])
circumSample (Samples samples) (Tolerance tolerance) r@(Radius limitRadius) ptCenter =
    unzip ys
    where
        (Epsilon eps) = defEps

        xs :: [TrueCourse]
        xs = [ TrueCourse $ ((2 * n) % samples) * F.pi eps | n <- [0 .. samples] ]

        circumR = circum ptCenter defEps

        ys = getClose 10 (Radius 0) (circumR r) <$> xs

        getClose :: Int -> Radius -> (TrueCourse -> LatLng) -> TrueCourse -> (LatLng, Double)
        getClose trys (Radius offset) f x
            | trys <= 0 = (y, dist)
            | tolerance <= 0 = (y, dist)
            | limitRadius <= tolerance = (y, dist)
            | otherwise =
                case d `compare` limitRadius of
                     EQ -> (y, dist)
                     GT ->
                        let offset' = offset - (d - limitRadius) * 105 / 100
                            f' = circumR (Radius $ limitRadius + offset')
                        in getClose (trys - 1) (Radius offset') f' x
                     LT ->
                        if d > limitRadius - tolerance then (y, dist) else
                            let offset' = offset + (limitRadius - d) * 94 / 100
                                f' = circumR (Radius $ limitRadius + offset')
                            in getClose (trys - 1) (Radius offset') f' x
            where
                y = f x
                (TaskDistance d) = distancePointToPoint [Point ptCenter, Point y]
                dist = fromRational d

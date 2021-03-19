module Flight.ShortestPath.Cost
    ( Zs(..)
    , PathCost(..)
    , OptimalCostedPath
    , GraphBuilder
    , NodeConnector
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , fromZs
    ) where

import Data.UnitsOfMeasure (u)
import Data.Graph.Inductive.Graph (Node, LEdge)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.LatLng (LatLng(..))
import Flight.Zone (Zone(..), ArcSweep(..))
import Flight.Zone.Cylinder (SampleParams(..), ZonePoint(..), CircumSample)
import Flight.Units ()
import Flight.Distance (PathDistance(..), SpanLatLng)

type OptimalCostedPath a = ([ZonePoint a], (PathCost a, [LatLng a [u| rad |]]))

type CostSegment a = Zone a -> Zone a -> PathDistance a

type NodeConnector a =
    [(Node, ZonePoint a)] -> [(Node, ZonePoint a)] -> [LEdge (PathCost a)]

type GraphBuilder a =
    CircumSample a
    -> SampleParams a
    -> ArcSweep a [u| rad |]
    -> Maybe [ZonePoint a]
    -> [Zone a]
    -> Gr (ZonePoint a) (PathCost a)

newtype PathCost a =
    PathCost a
    deriving (Eq, Ord)
    deriving newtype (Num, Real)

data Zs a
    = Zs a -- ^ All good, here's the wrapped value.
    | Z0 -- ^ No items when 2+ required.
    | Z1 -- ^ Only 1 item when 2+ required.
    | ZxNotSeparated -- ^ Zones are not separated.
    deriving (Eq, Ord, Functor)

deriving instance Show a => Show (Zs a)

fromZs :: Zs a -> Maybe a
fromZs (Zs a) = Just a
fromZs _ = Nothing

-- | A point to point distance with path function.
type DistancePointToPoint a = SpanLatLng a -> [Zone a] -> PathDistance a

-- | When searching some angles can be excluded. These are not in the initial
-- sweep. During the search the sweep angle is reduced by the next sweep
-- function.
data AngleCut a =
    AngleCut
        { sweep :: ArcSweep a [u| rad |]
        , nextSweep :: AngleCut a -> AngleCut a
        }

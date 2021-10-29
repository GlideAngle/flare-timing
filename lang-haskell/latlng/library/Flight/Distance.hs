{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Distance
    ( QTaskDistance
    , TaskDistance(..)
    , PathDistance(..)
    , SpanLatLng
    , unTaskDistanceAsKm
    , fromKms
    , toKm
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Convert (Convertible)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))

import Flight.LatLng (LatLng(..))
import Data.Ratio.Rounding (dpRound)
import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

-- | A function for measuring the distance between two points given as
-- latitude longitude pairs in radians.
type SpanLatLng a
    = LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> QTaskDistance a [u| m |]

fromKms
    :: Fractional a
    => Quantity a [u| km |]
    -> QTaskDistance a [u| m |]
fromKms q = TaskDistance (convert q)

-- | Convert to kilometres with mm accuracy.
toKm :: (Real a, Fractional a) => QTaskDistance a [u| m |] -> Double
toKm = toKm' (dpRound 6 . toRational)

toKm'
    :: Fractional a
    => (a -> Rational)
    -> QTaskDistance a [u| m |]
    -> Double
toKm' f (TaskDistance d) =
    fromRational $ f dKm
    where
        MkQuantity dKm = convert d :: Quantity _ [u| km |]

showDistance :: Quantity Rational [u| m |] -> String
showDistance d =
    show dbl
    where
        km = convert d :: Quantity Rational [u| km |]
        Flip rounded = dpRound 12 <$> Flip km
        dbl = fromRational' rounded :: Quantity Double [u| km |]

unTaskDistanceAsKm
    :: (Real a, Fractional a, Convertible u [u| m |])
    => QTaskDistance a u
    -> a
unTaskDistanceAsKm (TaskDistance d) =
    fromRational $ dpRound 6 dKm
    where
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

type QTaskDistance a u = TaskDistance (Quantity a u)
newtype TaskDistance a = TaskDistance a deriving (Eq, Ord, Generic)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (TaskDistance q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (TaskDistance q) q where
    pack = TaskDistance
    unpack (TaskDistance a) = a

instance
    (q ~ Quantity Double  [u| m |])
    => ToJSON (TaskDistance q) where
    toJSON (TaskDistance x) = toJSON $ ViaQ (TaskDistance y)
        where
            y :: Quantity Double [u| km |]
            y = convert x

instance
    (q ~ Quantity Double  [u| m |])
    => FromJSON (TaskDistance q) where
    parseJSON o = do
        ViaQ (TaskDistance x) <- parseJSON o
        return (TaskDistance $ convert x)

instance
    (Real a, Fractional a, q ~ Quantity a [u| m |])
    => Show (TaskDistance q) where
    show (TaskDistance d) = showDistance $ toRational' d

-- | The distance along a path of edges spanning vertices.
data PathDistance a =
    PathDistance
        { edgesSum :: QTaskDistance a [u| m |]
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone. An edge joins two vertices. These are summed to get
        -- the distance along the path that visits the vertices, each in turn.
        , vertices :: [ LatLng a [u| rad |] ]
        -- ^ The vertices that each edge spans.
        }

instance Eq a => Eq (PathDistance a) where
    a == b = (edgesSum a) == (edgesSum b) && (vertices a) == (vertices b)

instance Ord a => Ord (PathDistance a) where
    compare a b = compare (edgesSum a) (edgesSum b)

instance (Real a, Fractional a) => Show (PathDistance a) where
    show (PathDistance (TaskDistance d) _) = showDistance $ toRational' d

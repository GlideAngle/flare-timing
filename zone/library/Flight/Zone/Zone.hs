{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Zone.Zone
    ( HasArea(..), Zone(..), RawZoneToZone
    , center, radius, showZoneDMS, rawZonesToZones
    , toCylinder, rawToLatLng
    ) where

import Data.Foldable (asum)
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import Data.UnitsOfMeasure (u, convert, toRational', fromRational', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (fromQ)
import qualified Flight.LatLng.Raw as Raw (RawLat(..), RawLng(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), fromDMS)
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.Incline (Incline(..), QIncline)
import qualified Flight.Zone.Raw.Zone as Raw (RawZone(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))

-- | Does it have area?
class HasArea a where
    hasArea :: a -> Bool

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone a where
    -- | Used to mark the exact turnpoints in the optimized task distance.
    Point
        :: (Eq a, Ord a)
        => LatLng a [u| rad |]
        -> Zone a

    -- | Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    Vector
        :: (Eq a, Ord a)
        => QBearing a [u| rad |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | The turnpoint cylinder.
    Cylinder
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    Conical
        :: (Eq a, Ord a)
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | A goal line perpendicular to the course line.
    Line
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | This like a cylinder control zone but only used for goal.
    Circle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

deriving instance Eq (Zone a)
deriving instance Ord (Zone a)
deriving instance
    ( Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    , Show (LatLng a [u| rad |])
    )
    => Show (Zone a)

instance (Ord a, Num a) => HasArea (Zone a) where
    hasArea (Point _) = False
    hasArea (Vector _ _) = False
    hasArea (Cylinder (Radius x) _) = x > zero
    hasArea (Conical _ (Radius x) _) = x > zero
    hasArea (Line (Radius x) _) = x > zero
    hasArea (Circle (Radius x) _) = x > zero
    hasArea (SemiCircle (Radius x) _) = x > zero

instance
    ( ToJSON (LatLng a [u| rad |])
    , ToJSON (QBearing a [u| rad |])
    , ToJSON (QIncline a [u| rad |])
    , ToJSON (QRadius a [u| m |])
    )
    => ToJSON (Zone a) where
    toJSON (Point x) = object
        [ "point" .= toJSON x
        ]
    toJSON (Vector b x) = object
        [ "vector" .= object
            [ "bearing" .= toJSON b
            , "center" .= toJSON x
            ]
        ]
    toJSON (Cylinder r x) = object
        ["cylinder" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]
    toJSON (Conical i r x) = object
        [ "conical" .= object
            [ "radius" .= toJSON r
            , "incline" .= toJSON i
            , "center" .= toJSON x
            ]
        ]
    toJSON (Line r x) = object
        [ "line" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]
    toJSON (Circle r x) = object
        [ "circle" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

    toJSON (SemiCircle r x) = object
        [ "semicircle" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

instance
    ( Eq a
    , Ord a
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (Zone a) where
    parseJSON = withObject "Zone" $ \o ->
        asum
            [ Point <$> o .: "point"

            , do
                vc <- o .: "vector"
                Vector
                    <$> vc .: "bearing"
                    <*> vc .: "center"

            , do
                cy <- o .: "cylinder"
                Cylinder
                    <$> cy .: "radius"
                    <*> cy .: "center"

            , do
                co <- o .: "conical"
                Conical
                    <$> co .: "radius"
                    <*> co .: "incline"
                    <*> co .: "center"

            , do
                ln <- o .: "line"
                Line
                    <$> ln .: "radius"
                    <*> ln .: "center"

            , do
                cc <- o .: "circle"
                Circle
                    <$> cc .: "radius"
                    <*> cc .: "center"

            , do
                sc <- o .: "semicircle"
                SemiCircle
                    <$> sc .: "radius"
                    <*> sc .: "center"

            , fail "Unknown type of zone "
            ]

showZoneDMS :: Zone Double -> String
showZoneDMS (Point (LatLng (Lat x, Lng y))) =
    "Point " ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Bearing b) (LatLng (Lat x, Lng y))) =
    "Vector " ++ show (fromQ b) ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Cylinder r (LatLng (Lat x, Lng y))) =
    "Cylinder " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Conical (Incline i) r (LatLng (Lat x, Lng y))) =
    "Conical "
    ++ show (fromQ i)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

showZoneDMS (Line r (LatLng (Lat x, Lng y))) =
    "Line " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Circle r (LatLng (Lat x, Lng y))) =
    "Circle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (SemiCircle r (LatLng (Lat x, Lng y))) =
    "SemiCircle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

-- | The effective center point of a zone.
center :: Zone a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (Circle _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => Zone a -> QRadius a [u| m |]
radius (Point _) = Radius [u| 0m |]
radius (Vector _ _) = Radius [u| 0m |]
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (Circle r _) = r
radius (SemiCircle r _) = r

type RawZoneToZone =
    QRadius Double [u| m |] -> LatLng Double [u| rad |] -> Zone Double

rawZonesToZones
    :: RawZoneToZone
    -> [Raw.RawZone]
    -> [Zone Double]
rawZonesToZones goal xs =
    case reverse xs of
        (x : y : ys) -> reverse $ f goal x : (f Cylinder <$> (y : ys))
        _ -> f Cylinder <$> xs
    where
        f ctor Raw.RawZone{radius = r, lat, lng} =
            ctor r $ fromDMS (fromQ qLat, fromQ qLng)
                where
                    RawLat lat' = lat
                    RawLng lng' = lng

                    qLat :: Quantity Double [u| deg |]
                    qLat = fromRational' $ MkQuantity lat'

                    qLng :: Quantity Double [u| deg |]
                    qLng = fromRational' $ MkQuantity lng'

rawToLatLng :: Fractional a => Raw.RawLat -> Raw.RawLng -> LatLng a [u| rad |]
rawToLatLng (Raw.RawLat lat') (Raw.RawLng lng') =
    LatLng (Lat latRad, Lng lngRad)
    where
        latDeg :: Quantity _ [u| deg |]
        latDeg = MkQuantity $ fromRational lat'

        lngDeg :: Quantity _ [u| deg |]
        lngDeg = MkQuantity $ fromRational lng'

        latRad = convert latDeg :: Quantity _ [u| rad |]
        lngRad = convert lngDeg :: Quantity _ [u| rad |]

rawToRadius
    :: Fractional a
    => Radius (Quantity Double [u| m |])
    -> QRadius a [u| m |]
rawToRadius (Radius r) =
    Radius . fromRational' . toRational' $ r

toCylinder :: (Eq a, Ord a, Num a, Fractional a) => Raw.RawZone -> Zone a
toCylinder Raw.RawZone{Raw.radius = r, ..} =
    Cylinder
        (rawToRadius r)
        (rawToLatLng lat lng)

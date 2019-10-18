module Flight.Zone.Internal.ZoneKind
    ( HasArea(..)
    , ZoneKind(..)
    , Race
    , OpenDistance
    , CourseLine
    , Turnpoint
    , EndOfSpeedSection
    , Goal
    , EssAllowedZone
    , GoalAllowedZone
    , OpenAllowedZone
    , center
    , radius
    , showZoneDMS
    ) where

import Data.Foldable (asum)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.:), (.=)
    , object, withObject
    )
import Data.UnitsOfMeasure (u, zero)

import Flight.Units ()
import Flight.Units.DegMinSec (fromQ)
import Flight.LatLng (QAlt, Lat(..), Lng(..), LatLng(..))
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.AltTime (AltTime(..), QAltTime)
import Flight.Zone.Incline (Incline(..), QIncline)
import Flight.Zone.Zone (HasArea(..))

-- | A goal is a kind of a zone that is the very last zone, the end goal of
-- a race task. Not used in open distance tasks.
data Goal
    deriving
        ( EssAllowedZone
        , GoalAllowedZone
        )

-- | A kind of a zone that can end the speed section.
data EndOfSpeedSection
    deriving
        ( EssAllowedZone
        , GoalAllowedZone
        )

-- | The most general kind of a zone. Used in the prolog, race and epilog of
-- a race task, basically any zone of a race that is not at the end of the
-- speed section or goal. Also can be any zone in an open distance task. If
-- the open distance task has a heading then its last zone is not a regular
-- turnpoint.
data Turnpoint
    deriving
        ( OpenAllowedZone
        , EssAllowedZone
        )

-- | A kind of a zone only used in calculating the task length, a point.
data CourseLine

-- | A kind of a zone and a kind of a task. For zones, an open distance task
-- will this kind of zone as its last zone if the open distance section has
-- a heading along which the final leg is measured for open distance.
data OpenDistance
    deriving OpenAllowedZone

-- | A race is a kind of a task but not a kind of a zone.
data Race

class EssAllowedZone a
class GoalAllowedZone a
class OpenAllowedZone a

-- TODO: Remove standalone deriving Eq & Ord for empty data after GHC 8.4.1
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/7401
deriving instance Eq Goal
deriving instance Eq EndOfSpeedSection
deriving instance Eq Turnpoint
deriving instance Eq CourseLine
deriving instance Eq OpenDistance

deriving instance Ord Goal
deriving instance Ord EndOfSpeedSection
deriving instance Ord Turnpoint
deriving instance Ord CourseLine
deriving instance Ord OpenDistance

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data ZoneKind k a where
    -- | Used to mark the exact turnpoints in the optimized task distance.
    Point
        :: (Eq a, Ord a)
        => LatLng a [u| rad |]
        -> ZoneKind CourseLine a

    -- | Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    Vector
        :: (Eq a, Ord a, OpenAllowedZone k)
        => Either (LatLng a [u| rad |]) (QBearing a [u| rad |])
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | This is a cylinder control zone but only used for open distance. It
    -- marks the start of the open distance section in any direction.
    --
    -- A @Cylinder@ cannot be reused for open distance as it'd lead to these
    -- problems;
    --
    -- No instance for (OpenAllowedZone EndOfSpeedSection)
    --         arising from a use of ‘Cylinder’
    --
    -- No instance for (EssAllowedZone OpenDistance)
    --         arising from a use of ‘Cylinder’
    Star
        :: (Eq a, Ord a, OpenAllowedZone k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | The turnpoint cylinder.
    Cylinder
        :: (Eq a, Ord a, EssAllowedZone k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    CutCone
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> QAlt a [u| m |]
        -> ZoneKind k a

    -- | A halved cut cone.
    CutSemiCone
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> QAlt a [u| m |]
        -> ZoneKind k a

    -- | Only used in paragliding, this is an end of speed section used to
    -- discourage too low an end to final glides. This cylinder is cut to form
    -- a base. A time bonus is awarded proportional to the height of crossing
    -- above the cylinder's base.
    CutCylinder
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QAltTime a [u| s / m |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> QAlt a [u| m |]
        -> ZoneKind k a

    -- | A halved cylinder with a base.
    CutSemiCylinder
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QAltTime a [u| s / m |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> QAlt a [u| m |]
        -> ZoneKind k a

    -- | A goal line perpendicular to the course line.
    Line
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | This is a cylinder control zone but only used for goal.
    Circle
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle
        :: (Eq a, Ord a, EssAllowedZone k, GoalAllowedZone k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

deriving instance Eq (ZoneKind k a)
deriving instance
    ( Show (QAltTime a [u| s / m |])
    , Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    , Show (LatLng a [u| rad |])
    , Show (QAlt a [u| m |])
    )
    => Show (ZoneKind k a)

instance (Eq a, Ord a) => Ord (ZoneKind k a) where
    compare a b = center a `compare` center b

instance (Ord a, Num a) => HasArea (ZoneKind k a) where
    hasArea (Point _) = False
    hasArea (Vector _ (Radius x) _) = x > zero
    hasArea (Star (Radius x) _) = x > zero
    hasArea (Cylinder (Radius x) _) = x > zero
    hasArea (CutCone _ (Radius x) _ _) = x > zero
    hasArea (CutSemiCone _ (Radius x) _ _) = x > zero
    hasArea (CutCylinder _ (Radius x) _ _) = x > zero
    hasArea (CutSemiCylinder _ (Radius x) _ _) = x > zero
    hasArea (Line (Radius x) _) = x > zero
    hasArea (Circle (Radius x) _) = x > zero
    hasArea (SemiCircle (Radius x) _) = x > zero

instance
    ( ToJSON (QAlt a [u| m |])
    , ToJSON (LatLng a [u| rad |])
    , ToJSON (QBearing a [u| rad |])
    , ToJSON (QAltTime a [u| s / m |])
    , ToJSON (QIncline a [u| rad |])
    , ToJSON (QRadius a [u| m |])
    )
    => ToJSON (ZoneKind k a) where
    toJSON (Point x) = object
        [ "point" .= toJSON x ]

    toJSON (Vector (Left y) r x) = object
        [ "vector" .= object
            [ "target" .= toJSON y
            , "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

    toJSON (Vector (Right b) r x) = object
        [ "vector" .= object
            [ "bearing" .= toJSON b
            , "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

    toJSON (Star r x) = object
        ["star" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

    toJSON (Cylinder r x) = object
        ["cylinder" .= object
            [ "radius" .= toJSON r
            , "center" .= toJSON x
            ]
        ]

    toJSON (CutCone i r x a) = object
        [ "cut-cone" .= object
            [ "incline" .= toJSON i
            , "radius" .= toJSON r
            , "center" .= toJSON x
            , "altitude" .= toJSON a
            ]
        ]

    toJSON (CutSemiCone i r x a) = object
        [ "cut-semi-cone" .= object
            [ "incline" .= toJSON i
            , "radius" .= toJSON r
            , "center" .= toJSON x
            , "altitude" .= toJSON a
            ]
        ]

    toJSON (CutCylinder t r x a) = object
        ["cut-cylinder" .= object
            [ "time-bonus" .= toJSON t
            , "radius" .= toJSON r
            , "center" .= toJSON x
            , "altitude" .= toJSON a
            ]
        ]

    toJSON (CutSemiCylinder t r x a) = object
        ["cut-semi-cylinder" .= object
            [ "time-bonus" .= toJSON t
            , "radius" .= toJSON r
            , "center" .= toJSON x
            , "altitude" .= toJSON a
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
    => FromJSON (ZoneKind CourseLine a) where
    parseJSON = withObject "ZoneKind" $ \o -> 
        Point <$> o .: "point"

instance
    ( Eq a
    , Ord a
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind OpenDistance a) where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                vc <- o .: "vector"
                Vector . Left
                    <$> vc .: "target"
                    <*> vc .: "radius"
                    <*> vc .: "center"

            , do
                vc <- o .: "vector"
                Vector . Right
                    <$> vc .: "bearing"
                    <*> vc .: "radius"
                    <*> vc .: "center"

            , do
                cy <- o .: "star"
                Star
                    <$> cy .: "radius"
                    <*> cy .: "center"
            ]

instance
    ( Eq a
    , Ord a
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind Turnpoint a) where
    parseJSON = withObject "ZoneKind" $ \o -> do
        cy <- o .: "cylinder"
        Cylinder
            <$> cy .: "radius"
            <*> cy .: "center"

instance
    ( Eq a
    , Ord a
    , FromJSON (QAltTime a [u| s / m |])
    , FromJSON (QAlt a [u| m |])
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind EndOfSpeedSection a) where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                cy <- o .: "cylinder"
                Cylinder
                    <$> cy .: "radius"
                    <*> cy .: "center"

            , do
                co <- o .: "cut-cone"
                CutCone
                    <$> co .: "incline"
                    <*> co .: "radius"
                    <*> co .: "center"
                    <*> co .: "altitude"

            , do
                co <- o .: "cut-semi-cone"
                CutSemiCone
                    <$> co .: "incline"
                    <*> co .: "radius"
                    <*> co .: "center"
                    <*> co .: "altitude"

            , do
                cy <- o .: "cut-cylinder"
                CutCylinder
                    <$> cy .: "time-bonus"
                    <*> cy .: "radius"
                    <*> cy .: "center"
                    <*> cy .: "altitude"

            , do
                cy <- o .: "cut-semi-cylinder"
                CutSemiCylinder
                    <$> cy .: "time-bonus"
                    <*> cy .: "radius"
                    <*> cy .: "center"
                    <*> cy .: "altitude"
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
            ]

instance
    ( Eq a
    , Ord a
    , FromJSON (QAltTime a [u| s / m |])
    , FromJSON (QAlt a [u| m |])
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind Goal a) where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                co <- o .: "cut-cone"
                CutCone
                    <$> co .: "incline"
                    <*> co .: "radius"
                    <*> co .: "center"
                    <*> co .: "altitude"

            , do
                co <- o .: "cut-semi-cone"
                CutSemiCone
                    <$> co .: "incline"
                    <*> co .: "radius"
                    <*> co .: "center"
                    <*> co .: "altitude"

            , do
                cy <- o .: "cut-cylinder"
                CutCylinder
                    <$> cy .: "time-bonus"
                    <*> cy .: "radius"
                    <*> cy .: "center"
                    <*> cy .: "altitude"

            , do
                cy <- o .: "cut-semi-cylinder"
                CutSemiCylinder
                    <$> cy .: "time-bonus"
                    <*> cy .: "radius"
                    <*> cy .: "center"
                    <*> cy .: "altitude"
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

showZoneDMS :: ZoneKind k Double -> String
showZoneDMS (Point (LatLng (Lat x, Lng y))) =
    "Point " ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Left (LatLng (Lat bx, Lng by))) r (LatLng (Lat x, Lng y))) =
    "Vector "
    ++ show (fromQ bx, fromQ by)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Right (Bearing b)) r (LatLng (Lat x, Lng y))) =
    "Vector "
    ++ show (fromQ b)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

showZoneDMS (Star r (LatLng (Lat x, Lng y))) =
    "Star " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Cylinder r (LatLng (Lat x, Lng y))) =
    "Cylinder " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (CutCone (Incline i) r (LatLng (Lat x, Lng y)) a) =
    "Cut Cone "
    ++ show (fromQ i)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)
    ++ " "
    ++ show a

showZoneDMS (CutSemiCone (Incline i) r (LatLng (Lat x, Lng y)) a) =
    "Cut Semicone "
    ++ show (fromQ i)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)
    ++ " "
    ++ show a

showZoneDMS (CutCylinder (AltTime t) r (LatLng (Lat x, Lng y)) a) =
    "Cut Cylinder "
    ++ show t
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)
    ++ " "
    ++ show a

showZoneDMS (CutSemiCylinder (AltTime t) r (LatLng (Lat x, Lng y)) a) =
    "Cut Semicylinder "
    ++ show t
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)
    ++ " "
    ++ show a

showZoneDMS (Line r (LatLng (Lat x, Lng y))) =
    "Line " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Circle r (LatLng (Lat x, Lng y))) =
    "Circle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (SemiCircle r (LatLng (Lat x, Lng y))) =
    "Semicircle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

-- | The effective center point of a zone.
center :: ZoneKind k a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ _ x) = x
center (Star _ x) = x
center (Cylinder _ x) = x
center (CutCone _ _ x _) = x
center (CutSemiCone _ _ x _) = x
center (CutCylinder _ _ x _) = x
center (CutSemiCylinder _ _ x _) = x
center (Line _ x) = x
center (Circle _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => ZoneKind k a -> QRadius a [u| m |]
radius (Point _) = Radius [u| 0m |]
radius (Vector _ r _) = r
radius (Star r _) = r
radius (Cylinder r _) = r
radius (CutCone _ r _ _) = r
radius (CutSemiCone _ r _ _) = r
radius (CutCylinder _ r _ _) = r
radius (CutSemiCylinder _ r _ _) = r
radius (Line r _) = r
radius (Circle r _) = r
radius (SemiCircle r _) = r

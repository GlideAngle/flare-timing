module Flight.Zone.ZoneKind
    ( HasArea(..)
    , ZoneKind(..)
    , StartGates(..)
    , Task(..)
    , RaceTask
    , OpenDistanceTask
    , TaskZones(..)
    , RawZoneToZoneKind
    , Turnpoint
    , EndOfSpeedSection
    , Goal
    , EssAllowedZone
    , GoalAllowedZone
    , center
    , radius
    , showZoneDMS
    , rawZonesToZoneKinds
    ) where

import Data.Foldable (asum)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.:), (.=)
    , object, withObject
    )
import Data.UnitsOfMeasure (u, fromRational', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (fromQ)
import Flight.LatLng (QAlt, Lat(..), Lng(..), LatLng(..), fromDMS)
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.AltTime (AltTime(..), QAltTime)
import Flight.Zone.Incline (Incline(..), QIncline)
import qualified Flight.Zone.Raw.Zone as Raw (RawZone(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone.Zone (HasArea(..))

data Goal
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data EndOfSpeedSection
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data Turnpoint
    deriving (AnyZone, ZoneMaybeCylindrical)

data CourseLine
    deriving (AnyZone, ZoneMaybeCylindrical)

data OpenDistance
    deriving AnyZone

class AnyZone a where
class ZoneMaybeCylindrical a where
class EssAllowedZone a where
class GoalAllowedZone a where

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
        :: (Eq a, Ord a)
        => QBearing a [u| rad |]
        -> LatLng a [u| rad |]
        -> ZoneKind OpenDistance a

    -- | The turnpoint cylinder.
    Cylinder
        :: (Eq a, Ord a, ZoneMaybeCylindrical k)
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

    -- | This like a cylinder control zone but only used for goal.
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
    hasArea (Vector _ _) = False
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
    parseJSON = withObject "ZoneKind" $ \o -> do
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
    parseJSON = withObject "ZoneKind" $ \o -> do
        vc <- o .: "vector"
        Vector
            <$> vc .: "bearing"
            <*> vc .: "center"

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
            ]

instance
    ( Eq a
    , Ord a
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind Goal a) where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
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

            , fail $ "Unknown type of zone "
            ]

showZoneDMS :: ZoneKind k Double -> String
showZoneDMS (Point (LatLng (Lat x, Lng y))) =
    "Point " ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Bearing b) (LatLng (Lat x, Lng y))) =
    "Vector " ++ show (fromQ b) ++ " " ++ show (fromQ x, fromQ y)

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
center (Vector _ x) = x
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
radius (Vector _ _) = Radius [u| 0m |]
radius (Cylinder r _) = r
radius (CutCone _ r _ _) = r
radius (CutSemiCone _ r _ _) = r
radius (CutCylinder _ r _ _) = r
radius (CutSemiCylinder _ r _ _) = r
radius (Line r _) = r
radius (Circle r _) = r
radius (SemiCircle r _) = r

data RaceTask
data OpenDistanceTask

data TaskZones k a where

    EssIsGoal
        :: GoalAllowedZone g
        => [ZoneKind Turnpoint a]
        -> ZoneKind g a
        -> TaskZones RaceTask a

    EssIsNotGoal
        :: (EssAllowedZone e, GoalAllowedZone g)
        => [ZoneKind Turnpoint a]
        -> ZoneKind e a
        -> [ZoneKind Turnpoint a]
        -> ZoneKind g a
        -> TaskZones RaceTask a

    OpenDistanceTaskZones
        :: [ZoneKind Turnpoint a]
        -> ZoneKind OpenDistance a
        -> TaskZones OpenDistance a

instance Eq a => Eq (TaskZones k a) where
    (EssIsGoal a _) == (EssIsGoal b _) =
        a == b

    (EssIsNotGoal a _ _ _) == (EssIsNotGoal b _ _ _) =
        a == b

    (OpenDistanceTaskZones a _) == (OpenDistanceTaskZones b _) =
        a == b
    _ == _ = False

instance Ord a => Ord (TaskZones k a) where
    compare a b = turnpoints a `compare` turnpoints b

deriving instance (Show a, Show (LatLng a [u| rad |])) => Show (TaskZones k a)

turnpoints :: TaskZones k a -> [ZoneKind Turnpoint a]
turnpoints (EssIsGoal ts _) = ts
turnpoints (EssIsNotGoal ts _ us _) = ts ++ us
turnpoints (OpenDistanceTaskZones ts _) = ts

instance ToJSON (TaskZones k Double) where
    toJSON (EssIsGoal ts g) = object
        [ "turnpoints" .= toJSON ts
        , "goal" .= g
        ]

    toJSON (EssIsNotGoal ts e us g) = object
        [ "race" .= toJSON ts
        , "ess" .= e
        , "epilogue" .= toJSON us
        , "goal" .= g
        ]

    toJSON (OpenDistanceTaskZones ts o) = object
        [ "turnpoints" .= toJSON ts
        , "open" .= o
        ]

instance FromJSON (TaskZones OpenDistance Double) where
    parseJSON = withObject "OpenDistanceTask" $ \o -> do
        OpenDistanceTaskZones
        <$> o .: "open"
        <*> o .: "turnpoints"

instance FromJSON (TaskZones RaceTask Double) where
    parseJSON = withObject "RaceTask" $ \o ->
        asum
            [ do
                r :: [ZoneKind Turnpoint Double] <- o .: "race"
                e :: ZoneKind EndOfSpeedSection Double <- o .: "ess"
                p :: [ZoneKind Turnpoint Double] <- o .: "epilogue"
                g :: ZoneKind Goal Double <- o .: "goal"
                return $ EssIsNotGoal r e p g

            , do
                t :: [ZoneKind Turnpoint Double] <- o .: "turnpoints"
                g :: ZoneKind Goal Double <- o .: "goal"
                return $ EssIsGoal t g
            ]

type RawZoneToZoneKind k
    = QRadius Double [u| m |]
    -> LatLng Double [u| rad |]
    -> Maybe (QAlt Double [u| m |])
    -> ZoneKind k Double

rawZonesToZoneKinds
    :: (EssAllowedZone e, GoalAllowedZone g)
    => [RawZoneToZoneKind Turnpoint]
    -> RawZoneToZoneKind e
    -> [RawZoneToZoneKind Turnpoint]
    -> RawZoneToZoneKind g
    -> [Maybe (QAlt Double [u| m |])]
    -> [Raw.RawZone]
    -> Maybe (TaskZones RaceTask Double)
rawZonesToZoneKinds mkTps mkEss mkEps mkGoal as zs
    -- NOTE: Have not consumed all of the raw control zones.
    | not (null zs'''') || not (null as'''') = Nothing

    | null mkEps =
        case (gZs, gAs) of
            ([gZ], [gA]) ->
                Just $ EssIsGoal tps (f mkGoal gZ gA)

            _ -> Nothing

    | otherwise =
        case (eZs, eAs, gZs, gAs) of
            ([eZ], [eA], [gZ], [gA]) ->
                Just $ EssIsNotGoal tps (f mkEss eZ eA) eps (f mkGoal gZ gA)

            _ -> Nothing
    where
        splitTps :: [a] -> ([a], [a])
        splitTps = splitAt $ length mkTps

        splitEss :: [a] -> ([a], [a])
        splitEss = splitAt $ if null mkEps then 0 else 1

        splitEps :: [a] -> ([a], [a])
        splitEps = splitAt $ length mkEps

        splitGoal :: [a] -> ([a], [a])
        splitGoal = splitAt 1

        (tpZs, zs') = splitTps zs
        (tpAs, as') = splitTps as

        (eZs, zs'') = splitEss zs'
        (eAs, as'') = splitEss as'

        (epZs, zs''') = splitEps zs''
        (epAs, as''') = splitEps as''

        (gZs, zs'''') = splitGoal zs'''
        (gAs, as'''') = splitGoal as'''

        tps =
            [ f m z a
            | m <- mkTps
            | z <- tpZs
            | a <- tpAs 
            ]

        eps =
            [ f m z a
            | m <- mkEps
            | z <- epZs
            | a <- epAs 
            ]

        f ctor Raw.RawZone{radius = r, lat, lng} alt =
            ctor r (fromDMS (fromQ qLat, fromQ qLng)) alt
                where
                    RawLat lat' = lat
                    RawLng lng' = lng

                    qLat :: Quantity Double [u| deg |]
                    qLat = fromRational' $ MkQuantity lat'

                    qLng :: Quantity Double [u| deg |]
                    qLng = fromRational' $ MkQuantity lng'

newtype Deadline = Deadline Integer deriving (Eq, Ord, Show)
newtype TimeOfDay = TimeOfDay Rational deriving (Eq, Ord, Show)
newtype Interval = Interval Rational deriving (Eq, Ord, Show)

data StartGates
    = StartGates
        { open :: TimeOfDay
        , intervals :: [Interval]
        } deriving Show

data Task k a
    = Task
        { zones :: [ZoneKind k a]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }

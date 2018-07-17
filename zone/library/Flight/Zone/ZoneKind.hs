module Flight.Zone.ZoneKind
    ( HasArea(..)
    , ZoneKind(..)
    , StartGates(..)
    , Task(..)
    , RaceTask
    , OpenDistanceTask
    , TaskZones(..)
    , RawZoneToZoneKind
    , Goal
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
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), fromDMS)
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.Incline (Incline(..), QIncline)
import qualified Flight.Zone.Raw.Zone as Raw (RawZone(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone.Zone (HasArea(..))

data Goal
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data EndOfSpeedSection
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data Turnpoint
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

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
    Conical
        :: (Eq a, Ord a)
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind EndOfSpeedSection a

    -- | A goal line perpendicular to the course line.
    Line 
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind Goal a

    -- | This like a cylinder control zone but only used for goal.
    Circle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind Goal a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind Goal a

deriving instance Eq (ZoneKind k a)
deriving instance
    ( Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    , Show (LatLng a [u| rad |])
    )
    => Show (ZoneKind k a)

instance (Eq a, Ord a) => Ord (ZoneKind k a) where
    compare a b = center a `compare` center b

instance (Ord a, Num a) => HasArea (ZoneKind k a) where
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
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (ZoneKind EndOfSpeedSection a) where
    parseJSON = withObject "ZoneKind" $ \o -> do
        co <- o .: "conical"
        Conical
            <$> co .: "radius"
            <*> co .: "incline"
            <*> co .: "center"

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
center :: ZoneKind k a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (Circle _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => ZoneKind k a -> QRadius a [u| m |]
radius (Point _) = Radius [u| 0m |]
radius (Vector _ _) = Radius [u| 0m |]
radius (Cylinder r _) = r
radius (Conical _ r _) = r
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
    -> ZoneKind k Double

rawZonesToZoneKinds
    :: GoalAllowedZone k
    => RawZoneToZoneKind k
    -> [Raw.RawZone]
    -> Maybe (TaskZones RaceTask Double)
rawZonesToZoneKinds goal zs =
    case reverse zs of
        (x : xs) ->
            Just $ EssIsGoal (reverse $ f Cylinder <$> xs) (f goal x)
        _ -> Nothing
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

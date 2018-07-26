module Flight.Zone.ZoneKind
    ( HasArea(..)
    , ZoneKind(..)
    , StartGates(..)
    , Task(..)
    , Race
    , OpenDistance
    , TaskZones(..)
    , ToZoneKind
    , ToOpenZoneKind
    , Turnpoint
    , EndOfSpeedSection
    , Goal
    , EssAllowedZone
    , GoalAllowedZone
    , OpenAllowedZone
    , center
    , radius
    , showZoneDMS
    , raceZoneKinds
    , openZoneKinds
    ) where

import Data.Maybe (isNothing)
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

-- | A goal is a kind of a zone that is the very last zone, the end goal of
-- a race task. Not used in open distance tasks.
data Goal
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

-- | A kind of a zone that can end the speed section.
data EndOfSpeedSection
    deriving
        ( AnyZone
        , ZoneMaybeCylindrical
        , EssAllowedZone
        , GoalAllowedZone
        , OpenAllowedZone
-- TODO: Why is OpenAllowedZone is needed for EndOfSpeedSection?
-- /.../Fsdb/Task.hs:508:20: error:
--     • No instance for (ZK.OpenAllowedZone ZK.EndOfSpeedSection)
--         arising from a use of ‘ZK.Cylinder’
--     • In the expression: ZK.Cylinder r x
--       In a case alternative: Nothing -> ZK.Cylinder r x
--       In the expression:
--         \case
--           (Just alt) -> ZK.CutCone (mkIncline i) r x alt
--           Nothing -> ZK.Cylinder r x
--     |
-- 508 |         Nothing -> ZK.Cylinder r x
--     |                    ^^^^^^^^^^^^^^^
        )

-- | The most general kind of a zone. Used in the prolog, race and epilog of
-- a race task, basically any zone of a race that is not at the end of the
-- speed section or goal. Also can be any zone in an open distance task. If
-- the open distance task has a heading then its last zone is not a regular
-- turnpoint.
data Turnpoint
    deriving
        ( AnyZone
        , ZoneMaybeCylindrical
        , EssAllowedZone
        , OpenAllowedZone
        )

-- | A kind of a zone only used in calculating the task length, a point.
data CourseLine
    deriving
        ( AnyZone
        , ZoneMaybeCylindrical
        )

-- | A kind of a zone and a kind of a task. For zones, an open distance task
-- will this kind of zone as its last zone if the open distance section has
-- a heading along which the final leg is measured for open distance.
data OpenDistance
    deriving
        ( AnyZone
        , ZoneMaybeCylindrical
        , OpenAllowedZone
-- TODO: Why is EssAllowedZone needed for Open Distance?
-- /Fsdb/Task.hs:243:24: error:
--     • No instance for (ZK.EssAllowedZone ZK.OpenDistance)
--         arising from a use of ‘ZK.Cylinder’
--     • In the expression: ZK.Cylinder r x
--       In the expression: \ r x _ -> ZK.Cylinder r x
--       In an equation for ‘ok’: ok = \ r x _ -> ZK.Cylinder r x
--     |
-- 243 |         ok = \r x _ -> ZK.Cylinder r x
--     |                        ^^^^^^^^^^^^^^^
        , EssAllowedZone
        )

-- | A race is a kind of a task but not a kind of a zone.
data Race

class AnyZone a where
class ZoneMaybeCylindrical a where
class EssAllowedZone a where
class GoalAllowedZone a where
class OpenAllowedZone a where

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
        => QBearing a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> ZoneKind k a

    -- | The turnpoint cylinder.
    Cylinder
        :: (Eq a, Ord a, EssAllowedZone k, OpenAllowedZone k)
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
    hasArea (Vector _ (Radius x) _) = x > zero
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

    toJSON (Vector b r x) = object
        [ "vector" .= object
            [ "bearing" .= toJSON b
            , "radius" .= toJSON r
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
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                vc <- o .: "vector"
                Vector
                    <$> vc .: "bearing"
                    <*> vc .: "radius"
                    <*> vc .: "center"

            , do
                cy <- o .: "cylinder"
                Cylinder
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

showZoneDMS (Vector (Bearing b) r (LatLng (Lat x, Lng y))) =
    "Vector "
    ++ show (fromQ b)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

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
radius (Cylinder r _) = r
radius (CutCone _ r _ _) = r
radius (CutSemiCone _ r _ _) = r
radius (CutCylinder _ r _ _) = r
radius (CutSemiCylinder _ r _ _) = r
radius (Line r _) = r
radius (Circle r _) = r
radius (SemiCircle r _) = r

data TaskZones k a where

    TzEssIsGoal
        :: GoalAllowedZone g
        => [ZoneKind Turnpoint a] -- ^ prolog
        -> [ZoneKind Turnpoint a] -- ^ race
        -> ZoneKind g a -- ^ goal
        -> TaskZones Race a

    TzEssIsNotGoal
        :: (EssAllowedZone e, GoalAllowedZone g)
        => [ZoneKind Turnpoint a] -- ^ prolog
        -> [ZoneKind Turnpoint a] -- ^ race
        -> ZoneKind e a -- ^ race end
        -> [ZoneKind Turnpoint a] -- ^ epilog
        -> ZoneKind g a -- ^ goal
        -> TaskZones Race a

    TzOpenDistance
        :: OpenAllowedZone o
        => [ZoneKind Turnpoint a] -- ^ prolog
        -> [ZoneKind Turnpoint a] -- ^ mandatory
        -> ZoneKind o a -- ^ free
        -> TaskZones OpenDistance a

instance Eq a => Eq (TaskZones k a) where
    (TzEssIsGoal ap at _) == (TzEssIsGoal bp bt _) =
        ap == bp && at == bt

    (TzEssIsNotGoal ap at _ _ _) == (TzEssIsNotGoal bp bt _ _ _) =
        ap == bp && at == bt

    (TzOpenDistance ap at _) == (TzOpenDistance bp bt _) =
        ap == bp && at == bt

    _ == _ = False

instance Ord a => Ord (TaskZones k a) where
    compare a b = turnpoints a `compare` turnpoints b

deriving instance (Show a, Show (LatLng a [u| rad |])) => Show (TaskZones k a)

turnpoints :: TaskZones k a -> [ZoneKind Turnpoint a]
turnpoints (TzEssIsGoal ps ts _) = ps ++ ts
turnpoints (TzEssIsNotGoal ps ts _ us _) = ps ++ ts ++ us
turnpoints (TzOpenDistance ps ts _) = ps ++ ts

instance ToJSON (TaskZones k Double) where
    toJSON (TzEssIsGoal ps ts g) = object
        [ "prolog" .= toJSON ps
        , "race" .= toJSON ts
        , "race-ess-is-goal" .= g
        ]

    toJSON (TzEssIsNotGoal ps ts ess es g) = object
        [ "prolog" .= toJSON ps
        , "race" .= toJSON ts
        , "race-ess" .= ess
        , "epilog" .= toJSON es
        , "goal" .= g
        ]

    toJSON (TzOpenDistance ps ts o) = object
        [ "prolog" .= toJSON ps
        , "open-mandatory" .= toJSON ts
        , "open-free" .= o
        ]

instance FromJSON (TaskZones OpenDistance Double) where
    parseJSON = withObject "OpenDistance" $ \o ->
        asum
            [ do
                p :: [ZoneKind Turnpoint Double] <- o .: "prolog"
                m :: [ZoneKind Turnpoint Double] <- o .: "open-mandatory"
                f :: ZoneKind OpenDistance Double <- o .: "open-free"
                return $ TzOpenDistance p m f
            ]

instance FromJSON (TaskZones Race Double) where
    parseJSON = withObject "Race" $ \o ->
        asum
            [ do
                p :: [ZoneKind Turnpoint Double] <- o .: "prolog"
                r :: [ZoneKind Turnpoint Double] <- o .: "race"
                ess :: ZoneKind EndOfSpeedSection Double <- o .: "race-ess"
                epilog :: [ZoneKind Turnpoint Double] <- o .: "epilog"
                g :: ZoneKind Goal Double <- o .: "goal"
                return $ TzEssIsNotGoal p r ess epilog g

            , do
                p :: [ZoneKind Turnpoint Double] <- o .: "prolog"
                t :: [ZoneKind Turnpoint Double] <- o .: "race"
                g :: ZoneKind Goal Double <- o .: "race-ess-is-goal"
                return $ TzEssIsGoal p t g
            ]

type ToZoneKind k
    = QRadius Double [u| m |]
    -> LatLng Double [u| rad |]
    -> Maybe (QAlt Double [u| m |])
    -> ZoneKind k Double

type ToOpenZoneKind k
    = QRadius Double [u| m |]
    -> LatLng Double [u| rad |]
    -> Maybe (QAlt Double [u| m |])
    -> ZoneKind k Double

raceZoneKinds
    :: (EssAllowedZone e, GoalAllowedZone g)
    => [ToZoneKind Turnpoint] -- ^ prolog
    -> [ToZoneKind Turnpoint] -- ^ race
    -> Maybe (ToZoneKind e) -- ^ race end, if separate from goal
    -> [ToZoneKind Turnpoint] -- ^ epilog
    -> ToZoneKind g -- ^ goal
    -> [Maybe (QAlt Double [u| m |])]
    -> [Raw.RawZone]
    -> Maybe (TaskZones Race Double)
raceZoneKinds mkPps mkTps mkEss mkEps mkGoal as zs
    -- NOTE: Have not consumed all of the raw control zones.
    | not (null zs''''') || not (null as''''') = Nothing

    | isNothing mkEss && null mkEps =
        case (gZs, gAs) of
            ([gZ], [gA]) ->
                Just
                $ TzEssIsGoal pps tps (f mkGoal gZ gA)

            _ -> Nothing

    | otherwise =
        case (mkEss, eZs, eAs, gZs, gAs) of
            (Just mkEss', [eZ], [eA], [gZ], [gA]) ->
                Just
                $ TzEssIsNotGoal
                    pps
                    tps
                    (f mkEss' eZ eA)
                    eps
                    (f mkGoal gZ gA)

            _ -> Nothing
    where
        splitPps :: [a] -> ([a], [a])
        splitPps = splitAt $ length mkPps

        splitTps :: [a] -> ([a], [a])
        splitTps = splitAt $ length mkTps

        splitEss :: [a] -> ([a], [a])
        splitEss = splitAt $ maybe 0 (const 1) mkEss

        splitEps :: [a] -> ([a], [a])
        splitEps = splitAt $ length mkEps

        splitGoal :: [a] -> ([a], [a])
        splitGoal = splitAt 1

        (ppZs, zs') = splitPps zs
        (ppAs, as') = splitPps as

        (tpZs, zs'') = splitTps zs'
        (tpAs, as'') = splitTps as'

        (eZs, zs''') = splitEss zs''
        (eAs, as''') = splitEss as''

        (epZs, zs'''') = splitEps zs'''
        (epAs, as'''') = splitEps as'''

        (gZs, zs''''') = splitGoal zs''''
        (gAs, as''''') = splitGoal as''''

        pps =
            [ f m z a
            | m <- mkPps
            | z <- ppZs
            | a <- ppAs 
            ]

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

openZoneKinds
    :: (OpenAllowedZone o)
    => [ToOpenZoneKind Turnpoint] -- ^ prolog
    -> [ToOpenZoneKind Turnpoint] -- ^ mandatory
    -> ToOpenZoneKind o -- ^ open
    -> [Maybe (QAlt Double [u| m |])]
    -> [Raw.RawZone]
    -> Maybe (TaskZones OpenDistance Double)
openZoneKinds mkPps mkTps mkOpen as zs
    -- NOTE: Have not consumed all of the raw control zones.
    | not (null zs''') || not (null as''') = Nothing

    | otherwise =
        case (oZs, oAs) of
            ([oZ], [oA]) ->
                Just
                $ TzOpenDistance pps tps (f mkOpen oZ oA)

            _ -> Nothing

    where
        splitPps :: [a] -> ([a], [a])
        splitPps = splitAt $ length mkPps

        splitTps :: [a] -> ([a], [a])
        splitTps = splitAt $ length mkTps

        splitOpen :: [a] -> ([a], [a])
        splitOpen = splitAt 1

        (ppZs, zs') = splitPps zs
        (ppAs, as') = splitPps as

        (tpZs, zs'') = splitTps zs'
        (tpAs, as'') = splitTps as'

        (oZs, zs''') = splitOpen zs''
        (oAs, as''') = splitOpen as''

        pps =
            [ f m z a
            | m <- mkPps
            | z <- ppZs
            | a <- ppAs 
            ]

        tps =
            [ f m z a
            | m <- mkTps
            | z <- tpZs
            | a <- tpAs 
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

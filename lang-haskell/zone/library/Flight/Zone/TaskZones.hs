module Flight.Zone.TaskZones
    ( StartGates(..)
    , Task(..)
    , TaskZones(..)
    , ToZoneKind
    , ToOpenZoneKind
    , raceZoneKinds
    , openZoneKinds
    ) where

import Data.Maybe (isNothing)
import Data.Foldable (asum)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.:), (.=)
    , object, withObject
    )
import Data.UnitsOfMeasure (u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.String (IsString())

import Flight.Units ()
import Flight.Field (FieldOrdering(..))
import Flight.Units.DegMinSec (fromQ)
import Flight.LatLng (QAlt, LatLng(..), fromDMS)
import Flight.Zone.Radius (QRadius)
import qualified Flight.Zone.Raw.Zone as Raw (RawZone(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone.Internal.ZoneKind
    ( ZoneKind(..), Turnpoint, Goal, Race, OpenDistance, EndOfSpeedSection
    , OpenAllowedZone, EssAllowedZone, GoalAllowedZone, OpenAllowedZone
    )

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

splitToTurnpoints
    :: [p]
    -> [t]
    -> [a]
    -> [z]
    -> (([z], [a]), ([z], [a]), ([z], [a]))
splitToTurnpoints mkPps mkTps as zs =
    ((ppZs, ppAs), (tpZs, tpAs), (zs'', as''))
    where
        splitPps :: [a] -> ([a], [a])
        splitPps = splitAt $ length mkPps

        splitTps :: [a] -> ([a], [a])
        splitTps = splitAt $ length mkTps

        (ppZs, zs') = splitPps zs
        (ppAs, as') = splitPps as

        (tpZs, zs'') = splitTps zs'
        (tpAs, as'') = splitTps as'

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
    | not (null zs'''') || not (null as'''') = Nothing

    | isNothing mkEss && null mkEps =
        case (gZs, gAs) of
            ([gZ], [gA]) ->
                Just
                $ TzEssIsGoal pps tps (applyZ mkGoal gZ gA)

            _ -> Nothing

    | otherwise =
        case (mkEss, eZs, eAs, gZs, gAs) of
            (Just mkEss', [eZ], [eA], [gZ], [gA]) ->
                Just
                $ TzEssIsNotGoal
                    pps
                    tps
                    (applyZ mkEss' eZ eA)
                    eps
                    (applyZ mkGoal gZ gA)

            _ -> Nothing
    where
        splitEss :: [a] -> ([a], [a])
        splitEss = splitAt $ maybe 0 (const 1) mkEss

        splitEps :: [a] -> ([a], [a])
        splitEps = splitAt $ length mkEps

        splitGoal :: [a] -> ([a], [a])
        splitGoal = splitAt 1

        ((ppZs, ppAs), (tpZs, tpAs), (zs', as')) =
            splitToTurnpoints mkPps mkTps as zs

        (eZs, zs'') = splitEss zs'
        (eAs, as'') = splitEss as'

        (epZs, zs''') = splitEps zs''
        (epAs, as''') = splitEps as''

        (gZs, zs'''') = splitGoal zs'''
        (gAs, as'''') = splitGoal as'''

        pps =
            [ applyZ m z a
            | m <- mkPps
            | z <- ppZs
            | a <- ppAs 
            ]

        tps =
            [ applyZ m z a
            | m <- mkTps
            | z <- tpZs
            | a <- tpAs 
            ]

        eps =
            [ applyZ m z a
            | m <- mkEps
            | z <- epZs
            | a <- epAs 
            ]

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
    | not (null zs'') || not (null as'') = Nothing

    | otherwise =
        case (oZs, oAs) of
            ([oZ], [oA]) ->
                Just
                $ TzOpenDistance pps tps (applyZ mkOpen oZ oA)

            _ -> Nothing

    where
        splitOpen :: [a] -> ([a], [a])
        splitOpen = splitAt 1

        ((ppZs, ppAs), (tpZs, tpAs), (zs', as')) =
            splitToTurnpoints mkPps mkTps as zs

        (oZs, zs'') = splitOpen zs'
        (oAs, as'') = splitOpen as'

        pps =
            [ applyZ m z a
            | m <- mkPps
            | z <- ppZs
            | a <- ppAs 
            ]

        tps =
            [ applyZ m z a
            | m <- mkTps
            | z <- tpZs
            | a <- tpAs 
            ]

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

-- | Partially apply a function, a zone constructor, that takes radius, latlng
-- and altitude by extracting the radius and latlng from the raw zone. The
-- t type parameter here is the partially applied constructor without altitude
-- applied.
applyZ
    :: (QRadius Double [u| m |] -> LatLng Double [u| rad |] -> t)
    -> Raw.RawZone
    -> t
applyZ ctor Raw.RawZone{radius = r, lat, lng} =
    ctor r (fromDMS (fromQ qLat, fromQ qLng))
        where
            RawLat lat' = lat
            RawLng lng' = lng

            qLat :: Quantity Double [u| deg |]
            qLat = fromRational' $ MkQuantity lat'

            qLng :: Quantity Double [u| deg |]
            qLng = fromRational' $ MkQuantity lng'

instance FieldOrdering (TaskZones Race a) where
    fieldOrder _ = cmpTaskZonesRace

cmpTaskZonesRace :: (Ord a, IsString a) => a -> a -> Ordering
cmpTaskZonesRace a b =
    case (a, b) of
        ("prolog", _) -> LT

        ("race", "prolog") -> GT
        ("race", _) -> LT

        ("race-ess", "prolog") -> GT
        ("race-ess", "race") -> GT
        ("race-ess", _) -> LT

        ("epilog", "goal") -> LT
        ("epilog", _) -> GT

        ("goal", _) -> GT
        ("race-ess-is-goal", _) -> GT

        _ -> compare a b

instance FieldOrdering (TaskZones OpenDistance a) where
    fieldOrder _ = cmpTaskZonesOpenDistance

cmpTaskZonesOpenDistance :: (Ord a, IsString a) => a -> a -> Ordering
cmpTaskZonesOpenDistance a b =
    case (a, b) of
        ("prolog", _) -> LT

        ("open-mandatory", "open-free") -> LT
        ("open-mandatory", _) -> GT

        ("open-free", _) -> GT

        ("center", _) -> LT

        ("target", "radius") -> LT
        ("target", _) -> GT

        ("radius", _) -> GT

        _ -> compare a b

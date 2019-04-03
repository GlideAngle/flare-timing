module Flight.Zone.Raw.Zone
    ( RawZone(..)
    , Give(..)
    , showZone
    , zoneGive
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, (*:), (+:), (-:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt)
import Flight.LatLng.Raw (RawLat, RawLng, showLat, showLng)
import Flight.Zone.Radius (QRadius, Radius(..))

data Give =
    Give
        { giveFraction :: Double
        , giveDistance :: Maybe (QRadius Double [u| m |])
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RawZone =
    RawZone
        { zoneName :: String
        , lat :: RawLat
        , lng :: RawLng
        , radius :: QRadius Double [u| m |]
        , give :: Maybe (QRadius Double [u| m |])
        -- ^ There's a certain tolerance or give to the radius. This is the
        -- maximum of r + 5 m or 1.0005 * r for entry zones and a minimum of
        -- r - 5m or 0.9995 * r for exit zones as of 2018.
        , alt :: Maybe (QAlt Double [u| m |])
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

showZone :: RawZone -> String
showZone (RawZone name lat' lng' rad give alt') =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , show rad
            , show give
            , maybe "" show alt'
            ]

entry :: Give -> RawZone -> RawZone
entry
    Give{giveFraction = gf, giveDistance = Nothing}
    z@RawZone{radius = Radius r} =
    z{give = Just . Radius $ r *: (MkQuantity $ 1.0 + gf)}
entry
    Give{giveFraction = gf, giveDistance = Just (Radius dg)}
    z@RawZone{radius = Radius r} =
    let x = r *: (MkQuantity $ 1.0 + gf) in z{give = Just . Radius . max x $ r +: dg}

exit :: Give -> RawZone -> RawZone
exit
    Give{giveFraction = gf, giveDistance = Nothing}
    z@RawZone{radius = Radius r} =
    z{give = Just . Radius $ r *: (MkQuantity $ 1.0 - gf)}
exit
    Give{giveFraction = gf, giveDistance = Just (Radius dg)}
    z@RawZone{radius = Radius r} =
    let x = r *: (MkQuantity $ 1.0 - gf) in z{give = Just . Radius . min x $ r -: dg}

-- | Applies the give to each zone. Depending on whether the zone is an exit
-- zone or entry zone this shadowy zone be a cylinder smaller or larger than
-- the zone so that a pilot who nearly misses the zone might hit the zone with
-- added give.
zoneGive
    -- | This function should return true only if zones are separated and the
    -- first is not contained in the second.
    :: (RawZone -> RawZone -> Bool)
    -> Give
    -> [RawZone]
    -> [RawZone]
zoneGive _ _ [] = []
zoneGive _ g [x] = [exit g x]
zoneGive separatedZones g (x : xs) =
    scanl1 (f separatedZones g) (exit g x : xs)

-- | Give or turnpoint tolerance around an exit cylinder is troublesome. Pilots
-- will often get high outside the start and then fly back into the cylinder
-- before taking a start gate. In this way these cylinders are both exit and
-- entry cylinders. Give is not applied to exit cylinders until we find what
-- to do when applying the tolerance to them.
f :: (RawZone -> RawZone -> Bool) -> Give -> RawZone -> RawZone -> RawZone
f _ _ RawZone{give = Nothing} y = y
f separatedZones zg x@RawZone{give = Just _} y =
    if separatedZones x y then entry zg y else y

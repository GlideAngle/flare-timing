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

-- | There's a certain tolerance or give to the radius specified as a fraction
-- of the radius and minimum absolute amount that can be not specified
-- (effectively zero) or specified usually as zero or 5m. The line of the
-- boundary is then expanded into a band:
--
-- Taking for example a give fraction of 0.005 as the fraction and 5m as the
-- distance we have:
--
-- maximum of r + 5 m and 1.005 * r
-- minimum of r - 5 m and 0.995 * r
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
        , giveIn :: Maybe (QRadius Double [u| m |])
        -- ^ The effective give inside the zone proper.
        , giveOut :: Maybe (QRadius Double [u| m |])
        -- ^ The effective give outside the zone proper.
        , alt :: Maybe (QAlt Double [u| m |])
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

showZone :: RawZone -> String
showZone RawZone{..} =
    unwords [ zoneName
            , showLat lat
            , showLng lng
            , show radius
            , show (giveIn, giveOut)
            , maybe "" show alt
            ]

entryOrExit :: Give -> RawZone -> RawZone
entryOrExit
    Give{giveFraction = gf, giveDistance = Nothing}
    z@RawZone{radius = Radius r} =
    let fMin = ([u| 1.0 |] -: MkQuantity gf) *: r
        fMax = ([u| 1.0 |] +: MkQuantity gf) *: r
    in
        z
            { giveIn = Just . Radius $ fMin
            , giveOut = Just . Radius $ fMax
            }
entryOrExit
    Give{giveFraction = gf, giveDistance = Just (Radius dg)}
    z@RawZone{radius = Radius r} =
    let fMin = ([u| 1.0 |] -: MkQuantity gf) *: r
        fMax = ([u| 1.0 |] +: MkQuantity gf) *: r

        dMin = max [u| 0.0 m |] $ r -: dg
        dMax = r +: dg
    in
        z
            { giveIn = Just . Radius $ min fMin dMin
            , giveOut = Just . Radius $ max fMax dMax
            }

exit, entry :: Give -> RawZone -> RawZone
exit = entryOrExit
entry = entryOrExit

-- | Applies the give to each zone. Depending on whether the zone is an exit
-- zone or entry zone this shadowy zone be a cylinder smaller or larger than
-- the zone so that a pilot who nearly misses the zone might hit the zone with
-- added give.
--
-- >>> :{
-- zoneGive
--     (const $ const True)
--     Give{giveFraction = 0.005, giveDistance = Just $ Radius [u| 5m |]}
--     zs
-- :}
-- [RawZone {zoneName = "GS1", lat = 43.82973, lng = 16.64243, radius = Radius [u| 400.0 m |], giveIn = Just (Radius [u| 395.0 m |]), giveOut = Just (Radius [u| 405.0 m |]), alt = Just (Alt [u| 849.0 m |])},RawZone {zoneName = "G35", lat = 43.84411, lng = 16.6599, radius = Radius [u| 3000.0 m |], giveIn = Just (Radius [u| 2985.0 m |]), giveOut = Just (Radius [u| 3014.9999999999995 m |]), alt = Just (Alt [u| 933.0 m |])}]
zoneGive
    :: (RawZone -> RawZone -> Bool)
    -- ^ This function should return true only if zones are separated and the
    -- first is not contained in the second.
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
f _ _ RawZone{giveIn = Nothing, giveOut = Nothing} y = y
f separatedZones zg x@RawZone{} y =
    if separatedZones x y then entry zg y else y

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :set -fno-warn-partial-type-signatures
--
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
-- >>> import Flight.LatLng (Alt(..))
-- >>> import Flight.LatLng.Raw
--
-- >>> :{
-- zs :: [RawZone]
-- zs =
--     [ RawZone
--          { zoneName = "GS1"
--          , lat = RawLat $ toRational 43.82973
--          , lng = RawLng $ toRational 16.64243
--          , giveIn = Nothing
--          , giveOut = Nothing
--          , radius = Radius [u| 400 m |]
--          , alt = Just $ Alt [u| 849 m |]
--          }
--     , RawZone
--          { zoneName = "G35"
--          , lat = RawLat $ toRational 43.84411
--          , lng = RawLng $ toRational 16.6599
--          , giveIn = Nothing
--          , giveOut = Nothing
--          , radius = Radius [u| 3000 m |]
--          , alt = Just $ Alt [u| 933 m |]
--          }
--      ]
-- :}

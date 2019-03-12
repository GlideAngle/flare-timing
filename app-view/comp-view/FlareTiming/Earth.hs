module FlareTiming.Earth
    ( AzimuthFwd(..)
    , azimuthFwd
    , azimuthFlip
    ) where

import WireTypes.Zone (RawLat(..), RawLng(..), RawLatLng(..))

newtype AzimuthFwd = AzimuthFwd Double

-- SEE: https://www.movable-type.co.uk/scripts/latlong.html
azimuthFwd
    :: RawLatLng
    -> RawLatLng
    -> Maybe AzimuthFwd
azimuthFwd
    RawLatLng{lat = RawLat xLat, lng = RawLng xLng}
    RawLatLng{lat = RawLat yLat, lng = RawLng yLng} =
    Just . AzimuthFwd $ az
    where
        degToRad' = degToRad . fromRational
        xLat' = degToRad' xLat
        yLat' = degToRad' yLat
        xLng' = degToRad' xLng
        yLng' = degToRad' yLng

        deltaLng = yLng' - xLng'
        x = sin deltaLng * cos yLat'
        y = cos xLat' * sin yLat' - sin xLat' * cos yLat' * cos deltaLng
        az = radToDeg $ atan2 x y

degToRad :: Floating a => a -> a
degToRad a = a / 180 * pi

radToDeg :: Floating a => a -> a
radToDeg a = a / pi * 180

azimuthFlip :: AzimuthFwd -> AzimuthFwd
azimuthFlip (AzimuthFwd az) =
    AzimuthFwd . (fromIntegral :: Integer -> Double) $ round (az + 180) `rem` 360

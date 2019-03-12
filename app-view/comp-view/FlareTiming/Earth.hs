module FlareTiming.Earth (azimuthFwd) where

-- SEE: https://www.movable-type.co.uk/scripts/latlong.html
azimuthFwd
    :: RawLatLng
    -> RawLatLng
    -> Maybe Double
azimuthFwd
    RawLatLng{lat = RawLat xLatF, lng = RawLng xLngF}
    RawLatLng{lat = RawLat yLatF, lng = RawLng yLngF} =
    Just . degToRad $ atan2 x y
    where
        xLatF' = radToDeg xLatF
        yLatF' = radToDeg yLatF
        xLngF' = radToDeg xLngF
        yLngF' = radToDeg yLngF

        deltaLng = yLngF' - xLngF'
        x = sin deltaLng * cos yLatF'
        y = cos xLatF' * sin yLatF' - sin xLatF' * cos yLatF' * cos deltaLng

degToRad :: Floating a => a -> a
degToRad a = a / pi * 180

radToDeg :: Floating a => a -> a
radToDeg a = a / 180 * pi

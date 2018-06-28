{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Kml.Internal
    (
    -- ** Display of a fix
      showLatLngAlt
    , showLngLatAlt
    , showTimeAlt
    
    -- ** Parsing
    , formatFloat
    , roundTripLatLngAlt
    ) where

import Data.List.Split (splitOn)
import Numeric (showFFloat)
import Flight.Types
    ( Latitude(..), Longitude(..), Altitude(..), Seconds(..)
    , LLA(..), Fix(..)
    )

-- | Avoids __@"0."@__ because ...
-- 
-- @
-- > (read "0." :: Double)
-- Exception: Prelude.read: no parse
-- > (read "0.0" :: Double)
-- 0.0
-- @
--
-- >>> formatFloat "112.2334455"
-- "112.233446"
-- >>> formatFloat "0"
-- "0.000000"
-- >>> formatFloat "0."
-- "0.000000"
-- >>> formatFloat "0.0"
-- "0.000000"
formatFloat :: String -> String
formatFloat s =
    case splitOn "." s of
         [ a, "" ] -> showFFloat (Just 6) (read a :: Double) ""
         _ -> showFFloat (Just 6) (read s :: Double) ""

-- | Shows relative time offset in seconds and altitude in metres.
--
-- >>> import Flight.Kml (mkPosition)
-- >>> let lla = mkPosition (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- >>> showTimeAlt $ Fix (Seconds 0) lla Nothing
-- "(0s,214m)"
showTimeAlt :: Fix -> String
showTimeAlt Fix{fixMark, fix} =
    "(" ++ show s ++ "s," ++ show a ++ "m)"
    where
        Seconds s = fixMark
        LLA{llaAltGps} = fix
        Altitude a = llaAltGps

-- | Shows lat,lng,alt.
--
-- >>> showLatLngAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- "-33.650733,147.560367,214"
showLatLngAlt :: (Latitude, Longitude, Altitude) -> String
showLatLngAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lat :: Double)
            , ","
            , formatFloat $ show (fromRational lng :: Double)
            , ","
            , show alt
            ]

-- | Shows lng,lat,alt.
--
-- >>> showLngLatAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- "147.560367,-33.650733,214"
showLngLatAlt :: (Latitude, Longitude, Altitude) -> String
showLngLatAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lng :: Double)
            , ","
            , formatFloat $ show (fromRational lat :: Double)
            , ","
            , show alt
            ]

-- | Round trip from rational to double and back to rational.
-- 
-- >>> roundTripLatLngAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- (-33.650733,147.560367,214m)
roundTripLatLngAlt :: (Latitude, Longitude, Altitude)
                   -> (Double, Double, Altitude)
roundTripLatLngAlt (Latitude lat, Longitude lng, alt) =
    let lat' = read $ formatFloat $ show (fromRational lat :: Double)
        lng' = read $ formatFloat $ show (fromRational lng :: Double)
    in (lat', lng', alt)


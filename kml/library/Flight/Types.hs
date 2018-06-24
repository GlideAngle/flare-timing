{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Types
    ( Fix(..)
    , LLA(..)
    , LatLngAlt(..)
    , FixMark(..)
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    , MarkedFixes(..)
    , mkPosition
    , showTimeAlt
    , fixesLength
    , fixesSecondsRange
    , fixesUTCTimeRange
    , showFixesLength
    , showFixesSecondsRange
    , showFixesUTCTimeRange
    ) where

import Data.Time.Clock (UTCTime, addUTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.Via.Scientific
    ( DefaultDecimalPlaces(..)
    , deriveDecimalPlaces, toSci, showSci, dpDegree
    )

-- | Latitude in degress.
newtype Latitude = Latitude Rational
    deriving (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Longitude in degress.
newtype Longitude = Longitude Rational
    deriving (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Altitude in metres.
newtype Altitude = Altitude Integer
    deriving (Eq, Ord, Generic)
    deriving newtype Num
    deriving anyclass (ToJSON, FromJSON)

-- | The number of seconds offset from the time of the first fix.
newtype Seconds = Seconds Integer
    deriving (Eq, Ord, Generic)
    deriving newtype Num
    deriving anyclass (ToJSON, FromJSON)

deriveDecimalPlaces dpDegree ''Latitude
deriveDecimalPlaces dpDegree ''Longitude
deriveDecimalPlaces dpDegree ''Altitude
deriveDecimalPlaces dpDegree ''Seconds

instance Show Latitude where
    show x@(Latitude lat') =
        "lat=" ++ showSci dp (toSci dp lat')
            where
                dp = defdp x

instance Show Longitude where
    show x@(Longitude lng') =
        "lng=" ++ showSci dp (toSci dp lng')
            where
                dp = defdp x

instance Show Altitude where
    show (Altitude alt) = "alt=" ++ show alt

instance Show Seconds where
    show (Seconds sec) = "sec=" ++ show sec

-- | Latitude, longitude and GPS altitude.  Use 'mkPosition' to construct a 'LLA'.
data LLA =
    LLA
        { llaLat :: Latitude
        , llaLng :: Longitude
        , llaAltGps :: Altitude
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Constructs a 'LLA' from its parts.
-- 
-- >>> mkPosition (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- LLA {llaLat = lat=-33.65073300, llaLng = lng=147.56036700, llaAltGps = alt=214}
mkPosition :: (Latitude, Longitude, Altitude) -> LLA
mkPosition (lat', lng', alt') = LLA lat' lng' alt'

-- | Latitude, longitude and GPS altitude with a relative time offset in
-- seconds and possibly a barometric pressure altitude.
data Fix =
    Fix
        { fixMark :: Seconds
        -- ^ A mark in time, seconds offset from the first fix.
        , fix :: LLA
        -- ^ The coordinates of the fix, latitude, longitude and altitude.
        , fixAltBaro :: Maybe Altitude
        -- ^ The barometric pressure altitude of the fix.
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Shows relative time offset in seconds and altitude in metres.
--
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

-- | Class for a fix made up of latitude, longitude and GPS altitude.
class LatLngAlt a where
    lat :: a -> Latitude
    lng :: a -> Longitude
    altGps :: a -> Altitude

instance LatLngAlt LLA where
    lat LLA{llaLat} = llaLat
    lng LLA{llaLng} = llaLng
    altGps LLA{llaAltGps} = llaAltGps

instance LatLngAlt Fix where
    lat Fix{fix} = lat fix
    lng Fix{fix} = lng fix
    altGps Fix{fix} = altGps fix

-- | Class for a tracklog relative fix, offset in seconds, with an optional
-- barometric pressure altitude.
class LatLngAlt a => FixMark a where
    -- | Seconds offset from first fix.
    mark :: a -> Seconds
    -- | Barometric pressure altitude.
    altBaro :: a -> Maybe Altitude

instance FixMark Fix where
    mark Fix{fixMark} = fixMark
    altBaro Fix{fixAltBaro} = fixAltBaro

-- | A tracklog is a list of fixes along with the UTC time of the first fix.
data MarkedFixes =
    MarkedFixes
        { mark0 :: UTCTime -- ^ The UTC time of the first fix.
        , fixes :: [Fix] -- ^ The fixes of the track log.
        }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The number of fixes in the track log.
fixesLength :: MarkedFixes -> Int
fixesLength MarkedFixes{fixes} =
    length fixes

-- | In the given list of fixes, the seconds offset of the first and last
-- elements.
fixesSecondsRange :: MarkedFixes -> Maybe (Seconds, Seconds)
fixesSecondsRange MarkedFixes{fixes} =
    case (fixes, reverse fixes) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (x : _, y : _) -> Just (mark x, mark y)

-- | In the given list of fixes, the UTC time of the first and last elements.
fixesUTCTimeRange :: MarkedFixes -> Maybe (UTCTime, UTCTime)
fixesUTCTimeRange mf@MarkedFixes{mark0} =
    rangeUTCTime mark0 <$> fixesSecondsRange mf

-- | Shows the number of elements in the list of fixes, in the tracklog.
showFixesLength :: MarkedFixes -> String
showFixesLength = show . fixesLength

-- | Shows the relative time range of the tracklog.
showFixesSecondsRange :: MarkedFixes -> String
showFixesSecondsRange mf =
    maybe "[]" show (fixesSecondsRange mf)

-- | Shows the absolute time range of the tracklog.
showFixesUTCTimeRange :: MarkedFixes -> String
showFixesUTCTimeRange mf@MarkedFixes{mark0} =
    maybe "" (show . rangeUTCTime mark0) (fixesSecondsRange mf)

-- | By providing the UTC time of the first fix, convert a relative time range of offset seconds into a time absolute time range of UTC times.
rangeUTCTime :: UTCTime -> (Seconds, Seconds) -> (UTCTime, UTCTime)
rangeUTCTime mark0 (Seconds s0, Seconds s1) =
    let f secs = fromInteger secs `addUTCTime` mark0 in (f s0, f s1)

﻿module Flight.Types
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
    , timeToFixIdx
    , secondsToUtc
    , fixToUtc
    ) where

import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.List (findIndex, findIndices)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Clip (FlyCut(..), FlyClipping(..), FlyClipSection(..))

import Data.Via.Scientific
    ( DefaultDecimalPlaces(..)
    , deriveDecimalPlaces, toSci, showSci, dpDegree
    )

-- | Latitude in degress.
newtype Latitude = Latitude Rational
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Longitude in degress.
newtype Longitude = Longitude Rational
    deriving (Eq, Ord, Generic)
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
        showSci dp (toSci dp lat') ++ "°"
            where
                dp = defdp x

instance Show Longitude where
    show x@(Longitude lng') =
        showSci dp (toSci dp lng') ++ "°"
            where
                dp = defdp x

instance Show Altitude where
    show (Altitude alt) = show alt ++ "m"

instance Show Seconds where
    show (Seconds sec) = show sec ++ "s"

-- | Latitude, longitude and GPS altitude.  Use 'mkPosition' to construct a 'LLA'.
data LLA =
    LLA
        { llaLat :: Latitude
        , llaLng :: Longitude
        , llaAltGps :: Altitude
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Constructs a 'LLA' from its parts.
-- 
-- >>> mkPosition (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- LLA {llaLat = -33.65073300°, llaLng = 147.56036700°, llaAltGps = 214m}
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
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

betweenFixMark :: FixMark a => Seconds -> Seconds -> a -> Bool
betweenFixMark s0 s1 x =
    let s = mark x in s0 <= s && s <= s1

instance FlyClipping UTCTime MarkedFixes where
    clipToFlown x@FlyCut{cut = Nothing, uncut} =
        x{uncut = uncut{fixes = []}}

    clipToFlown x@FlyCut{cut = Just (t0, t1), uncut = mf@MarkedFixes{mark0, fixes}} =
        x{uncut = mf{fixes = filter (betweenFixMark s0 s1) fixes}}
        where
            s0 = Seconds . round $ t0 `diffUTCTime` mark0
            s1 = Seconds . round $ t1 `diffUTCTime` mark0

    clipIndices FlyCut{cut = Nothing} = []

    clipIndices FlyCut{cut = Just (t0, t1), uncut = MarkedFixes{mark0, fixes}} =
        findIndices (betweenFixMark s0 s1) fixes
        where
            s0 = Seconds . round $ t0 `diffUTCTime` mark0
            s1 = Seconds . round $ t1 `diffUTCTime` mark0

instance FlyClipSection UTCTime MarkedFixes Int where
    clipSection x =
        case (xs, reverse xs) of
            ([], _) -> Nothing
            (_, []) -> Nothing
            (i : _, j : _) -> Just (i, j)
        where
            xs = clipIndices x

-- | Finds the 0-based index of the fix.
timeToFixIdx :: UTCTime -> MarkedFixes -> Maybe Int
timeToFixIdx t MarkedFixes{mark0, fixes} =
    findIndex ((== s) . fixMark) fixes
    where
        s = Seconds . round $ t `diffUTCTime` mark0

secondsToUtc :: UTCTime -> Seconds -> UTCTime
secondsToUtc mark0 (Seconds secs) =
    fromInteger secs `addUTCTime` mark0

fixToUtc :: UTCTime -> Fix -> UTCTime
fixToUtc mark0 x =
    secondsToUtc mark0 $ mark x

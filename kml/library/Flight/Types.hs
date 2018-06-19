﻿{-# LANGUAGE DeriveGeneric #-}
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

newtype Latitude = Latitude Rational
    deriving (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Longitude = Longitude Rational
    deriving (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Altitude = Altitude Integer
    deriving (Eq, Ord, Generic)
    deriving newtype Num
    deriving anyclass (ToJSON, FromJSON)

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

data LLA =
    LLA
        { llaLat :: Latitude
        , llaLng :: Longitude
        , llaAltGps :: Altitude
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Fix =
    Fix
        { fixMark :: Seconds
        , fix :: LLA
        , fixAltBaro :: Maybe Altitude
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

showTimeAlt :: Fix -> String
showTimeAlt Fix{fixMark, fix} =
    "(" ++ show s ++ "s," ++ show a ++ "m)"
    where
        Seconds s = fixMark
        LLA{llaAltGps} = fix
        Altitude a = llaAltGps

mkPosition :: (Latitude, Longitude, Altitude) -> LLA
mkPosition (lat', lng', alt') = LLA lat' lng' alt'

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

class LatLngAlt a => FixMark a where
    mark :: a -> Seconds
    altBaro :: a -> Maybe Altitude

instance FixMark Fix where
    mark Fix{fixMark} = fixMark
    altBaro Fix{fixAltBaro} = fixAltBaro

data MarkedFixes =
    MarkedFixes
        { mark0 :: UTCTime
        , fixes :: [Fix]
        }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

fixesLength :: MarkedFixes -> Int
fixesLength MarkedFixes{fixes} =
    length fixes

fixesSecondsRange :: MarkedFixes -> Maybe (Seconds, Seconds)
fixesSecondsRange MarkedFixes{fixes} =
    case (fixes, reverse fixes) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (x : _, y : _) -> Just (mark x, mark y)

fixesUTCTimeRange :: MarkedFixes -> Maybe (UTCTime, UTCTime)
fixesUTCTimeRange mf@MarkedFixes{mark0} =
    rangeUTCTime mark0 <$> fixesSecondsRange mf

showFixesLength :: MarkedFixes -> String
showFixesLength = show . fixesLength

showFixesSecondsRange :: MarkedFixes -> String
showFixesSecondsRange mf =
    maybe "[]" show (fixesSecondsRange mf)

showFixesUTCTimeRange :: MarkedFixes -> String
showFixesUTCTimeRange mf@MarkedFixes{mark0} =
    maybe "" (show . rangeUTCTime mark0) (fixesSecondsRange mf)

rangeUTCTime :: UTCTime -> (Seconds, Seconds) -> (UTCTime, UTCTime)
rangeUTCTime mark0 (Seconds s0, Seconds s1) =
    let f secs = fromInteger secs `addUTCTime` mark0 in (f s0, f s1)

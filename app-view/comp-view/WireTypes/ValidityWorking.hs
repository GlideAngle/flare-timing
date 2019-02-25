{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.ValidityWorking
    ( ValidityWorking(..)
    -- * Launch Validity Working
    , LaunchValidityWorking(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    -- * Distance Validity Working
    , DistanceValidityWorking(..)
    , SumOfDistance(..)
    , NominalDistanceArea(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , MaximumDistance(..)
    -- * Time Validity Working
    , TimeValidityWorking(..)
    , BestTime(..)
    , BestDistance(..)
    , NominalTime(..)
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import qualified Data.Text as T (unpack)
import Data.Aeson (Value(..), FromJSON(..))

import FlareTiming.Time (showHmsForHours)

data ValidityWorking =
    ValidityWorking 
        { launch :: LaunchValidityWorking
        , distance :: DistanceValidityWorking
        , time :: TimeValidityWorking
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data LaunchValidityWorking =
    LaunchValidityWorking 
        { flying :: PilotsFlying
        , present :: PilotsPresent
        , nominalLaunch :: NominalLaunch
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype PilotsPresent = PilotsPresent Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype PilotsFlying = PilotsFlying Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype NominalLaunch = NominalLaunch Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

data DistanceValidityWorking =
    DistanceValidityWorking 
        { sum :: SumOfDistance
        , flying :: PilotsFlying
        , area :: NominalDistanceArea
        , nominalGoal :: NominalGoal
        , nominalDistance :: NominalDistance
        , minimumDistance :: MinimumDistance
        , bestDistance :: MaximumDistance
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype SumOfDistance = SumOfDistance Double
    deriving (Eq, Ord)

newtype NominalDistanceArea = NominalDistanceArea Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype NominalGoal = NominalGoal Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype NominalDistance = NominalDistance Double
    deriving (Eq, Ord)

newtype MinimumDistance = MinimumDistance Double
    deriving (Eq, Ord)

newtype MaximumDistance = MaximumDistance Double
    deriving (Eq, Ord)

instance Show SumOfDistance where
    show (SumOfDistance x) = show x ++ " km"

instance Show NominalDistance where
    show (NominalDistance x) = show x ++ " km"

instance Show MinimumDistance where
    show (MinimumDistance x) = show x ++ " km"

instance Show MaximumDistance where
    show (MaximumDistance x) = show x ++ " km"

instance FromJSON SumOfDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . SumOfDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON NominalDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . NominalDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON MinimumDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . MinimumDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON MaximumDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . MaximumDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data TimeValidityWorking =
    TimeValidityWorking 
        { ssBestTime :: Maybe BestTime
        , gsBestTime :: Maybe BestTime
        , bestDistance :: BestDistance
        , nominalTime :: NominalTime
        , nominalDistance :: NominalDistance
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

newtype BestTime = BestTime Double
    deriving (Eq, Ord)

newtype BestDistance = BestDistance Double
    deriving (Eq, Ord)

newtype NominalTime = NominalTime Double
    deriving (Eq, Ord)

instance Show BestTime where
    show (BestTime x) = T.unpack $ showHmsForHours x

instance Show BestDistance where
    show (BestDistance x) = show x ++ " km"

instance Show NominalTime where
    show (NominalTime x) = show x ++ " h"

instance FromJSON BestTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . BestTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON BestDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . BestDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON NominalTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . NominalTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

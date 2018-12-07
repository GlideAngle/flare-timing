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
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (Value(..), FromJSON(..))

data ValidityWorking =
    ValidityWorking 
        { launch :: LaunchValidityWorking
        , distance :: DistanceValidityWorking
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

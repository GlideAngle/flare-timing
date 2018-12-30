{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ExtractInputOptions
    ( CmdOptions(..)
    , mkOptions
    , mkEarthModel
    ) where

import Text.RawString.QQ (r)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , name
    , help
    , (&=)
    , explicit
    , typ
    )

import Flight.Comp (Projection(..), EarthModel(..))
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)

data Earth
    = Sphere
    | Ellipsoid
    | Flat
    deriving (Show, Data, Typeable)

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions
        { dir :: FilePath
        -- ^ Picking all competition in this directory.
        , file :: FilePath
        -- ^ Picking the competition in this file.
        , giveFraction :: Maybe Double
        -- ^ How much give (as a fraction) is there when crossing zones?
        , giveDistance :: Maybe Double
        -- ^ How much give (in metres) is there when crossing zones?
        , earth :: Maybe Earth
        }
    deriving (Show, Data, Typeable)

description :: String
description = [r|
Extracts just the inputs needed for scoring a competition.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.fsdb
    Writes ./c.comp-inputs.yaml

The *.fsdb does not hold zone tolerances or give and these must be supplied.
Note that the give fraction is required but the give distance is optional.
|]

mkOptions :: String -> CmdOptions
mkOptions programName =
    CmdOptions
        { dir = def
        &= help "Over all FSDB files in this directory"
        &= groupname "Source"

        , file = def
        &= help "With this one competition FSDB file"
        &= groupname "Source"

        , earth = def
        &= typ "sphere|ellipsoid|flat"
        &= help "Which Earth model?"

        , giveFraction = def
        &= explicit
        &= name "give-fraction"
        &= help "How much give as a fraction, eg. 0.05% = 0.0005?"
        &= groupname "Give when crossing zones"

        , giveDistance = def
        &= explicit
        &= name "give-distance"
        &= help "How much give in metres?"
        &= groupname "Give when crossing zones"
        }
        &= summary description
        &= program programName

mkEarthModel :: Earth -> EarthModel
mkEarthModel = \case
    Sphere -> EarthAsSphere earthRadius
    Ellipsoid -> EarthAsEllipsoid wgs84
    Flat -> EarthAsFlat UTM

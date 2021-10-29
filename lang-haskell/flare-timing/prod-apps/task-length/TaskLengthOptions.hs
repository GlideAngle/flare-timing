{-- WARNING set -XNoOverloadedStrings to avoid errors with -XDeriveDataTypeable
/ error:
    • No instance for (Typeable a0) arising from a use of ‘opt’
    • In the second argument of ‘(&=)’, namely ‘opt "name"’
      In the first argument of ‘(&=)’, namely
        ‘def &= help "Which tasks?" &= typ "TASK NUMBER" &= opt "name"’
      In the ‘task’ field of a record
    |
100 |         &= opt "name"
    |            ^^^^^^^^^^

/ error:
    • Ambiguous type variable ‘a0’ arising from the literal ‘"name"’
      prevents the constraint ‘(Data.String.IsString
                                  a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance a ~ Char => Data.String.IsString [a]
          -- Defined in ‘Data.String’
        ...plus 9 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘opt’, namely ‘"name"’
      In the second argument of ‘(&=)’, namely ‘opt "name"’
      In the first argument of ‘(&=)’, namely
        ‘def &= help "Which tasks?" &= typ "TASK NUMBER" &= opt "name"’
    |
100 |         &= opt "name"
--}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TaskLengthOptions (CmdOptions(..), mkOptions) where

import Text.RawString.QQ (r)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , typ
    , name
    , opt
    , explicit
    , enum
    , help
    , (&=)
    )
import Flight.Geodesy (EarthMath(..))
import Flight.Route (TaskDistanceMeasure(..))

deriving instance Data EarthMath
deriving instance Typeable EarthMath
deriving instance Data TaskDistanceMeasure

instance Default TaskDistanceMeasure where
    def = TaskDistanceByAllMethods

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions
        { file :: FilePath
        -- ^ Picking the competition in this file.
        , task :: [Int]
        -- ^ Include only these tasks.
        , measure :: TaskDistanceMeasure
        -- ^ Use the given measure(s).
        , noTaskWaypoints :: Bool
        -- ^ Exclude task waypoints
        , earthMath :: EarthMath
        }
    deriving (Data, Typeable, Show)

description :: String
description = [r|
Works out the task length by following an optimal route.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.comp-inputs.yaml
    Writes ./c.task-length.yaml
|]

mkOptions :: String -> CmdOptions
mkOptions programName =
    CmdOptions
        { file = def
        &= help "With this one competition *.comp.yaml file"
        &= groupname "Source"

        , task = def
        &= help "Which tasks?"
        &= typ "TASK NUMBER"
        &= opt "name"
        &= groupname "Filter"

        , measure = def
        &= help "Which way to measure task distances, taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges"
        &= typ "METHOD"
        &= groupname "Filter"

        , earthMath =
            enum
                [ Pythagorus
                &= help "Pythagorus method on a plane"
                , Haversines
                &= help "Haversines on a sphere"
                , Vincenty
                &= help "Vincenty method on an ellipsoid"
                , AndoyerLambert
                &= help "Andoyer-Lambert method on an ellipsoid"
                , ForsytheAndoyerLambert
                &= help "Forsythe-Andoyer-Lambert method on an ellipsoid"
                ]
        &= groupname "Earth math"

        , noTaskWaypoints = def
        &= help "Exclude the task waypoints?"
        &= explicit
        &= name "no-task-waypoints"
        &= groupname "Filter"
        }
        &= summary description
        &= program programName

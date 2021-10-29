{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions) where

import Data.Maybe (fromMaybe)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , typ
    , opt
    , help
    , (&=)
    )

import Flight.Span.Math (Math(..))
import Flight.Cmd.Options (ProgramName(..), Description(..), Extension(..))

-- | Options passed in on the command line.
data CmdBatchOptions
    = CmdBatchOptions
        { file :: FilePath
        -- ^ Picking the competition in this file.
        , task :: [Int]
        -- ^ Include only these tasks.
        , pilot :: [String]
        -- ^ Look only at these pilots
        , math :: Math
        -- ^ The kind of numbers to do math with?
        , speedSectionOnly :: Bool
        -- ^ Exclude legs outside of the speed section?
        }
    deriving (Data, Typeable, Show)

mkOptions :: ProgramName -> Description -> Maybe Extension -> CmdBatchOptions
mkOptions (ProgramName programName) (Description description) ext =
    CmdBatchOptions
        { file = def
        &= help fileMsg
        &= groupname "Source"

        , task = def
        &= help "Which tasks?"
        &= typ "TASK NUMBER"
        &= opt "name"
        &= groupname "Filter"

        , pilot = def
        &= help "Which pilots?"
        &= typ "PILOT NAME"
        &= opt "name"
        &= groupname "Filter"

        , math = def
        &= help "Do math with which kind of numbers?"
        &= typ "rational|floating"
        &= opt "name"
        &= groupname "Precision"

        , speedSectionOnly = def
        &= help "Exclude legs outside of the speed section?"
        &= groupname "Filter"

        }
        &= summary description
        &= program programName
    where
        Extension ext' = fromMaybe (Extension "*.comp.yaml") ext
        fileMsg = "With this one competition " ++ ext' ++ " file"

{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions) where

import Data.Maybe (fromMaybe)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , help
    , (&=)
    )

import Flight.Cmd.Options (ProgramName(..), Description(..), Extension(..))

-- | Options passed in on the command line.
data CmdServeOptions
    = CmdServeOptions
        { dir :: FilePath
        -- ^ Picking all competition in this directory.
        , file :: FilePath
        -- ^ Picking the competition in this file.
        }
    deriving (Data, Typeable, Show)

mkOptions :: ProgramName -> Description -> Maybe Extension -> CmdServeOptions
mkOptions (ProgramName programName) (Description description) ext =
    CmdServeOptions
        { dir = def
        &= help dirMsg
        &= groupname "Source"

        , file = def
        &= help fileMsg
        &= groupname "Source"

        }
        &= summary description
        &= program programName
    where
        Extension ext' = fromMaybe (Extension "*.comp.yaml") ext
        dirMsg =  "Over all " ++ ext' ++ " files in this directory"
        fileMsg = "With this one competition " ++ ext' ++ " file"

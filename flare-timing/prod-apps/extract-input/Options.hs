{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Options (CmdOptions(..), mkOptions) where

import Text.RawString.QQ (r)
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

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
                 deriving (Show, Data, Typeable)

description :: String
description = [r|
Extracts just the inputs needed for scoring a competition.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.fsdb
    Writes ./c.comp-inputs.yaml
|]

mkOptions :: String -> CmdOptions
mkOptions programName =
    CmdOptions
        { dir = def
        &= help "Over all the competition FSDB files in this directory"
        &= groupname "Source"

        , file = def
        &= help "With this one competition FSDB file"
        &= groupname "Source"
        }
        &= summary description
        &= program programName

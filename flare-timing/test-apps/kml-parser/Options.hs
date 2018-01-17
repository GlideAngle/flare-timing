{-# LANGUAGE DeriveDataTypeable #-}

module Options (KmlOptions(..), mkOptions) where

import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , help
    , (&=)
    )

-- | Options passed in on the command line.
data KmlOptions
    = KmlOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
                 deriving (Show, Data, Typeable)

description :: String
description = "A parser of KML, the Keyhole Markup Language, an XML format."

mkOptions :: String -> KmlOptions
mkOptions programName =
    KmlOptions
        { dir = def &= help "Over all the KML files in this directory"
        , file = def &= help "With this one KML file"
        }
        &= summary description
        &= program programName

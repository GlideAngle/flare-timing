{-# LANGUAGE DeriveDataTypeable #-}

module KmlOptions (KmlOptions(..), mkOptions) where

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
    = KmlOptions
        { file :: FilePath
        -- ^ Picking the competition in this file.
        }
    deriving (Show, Data, Typeable)

description :: String
description = "A parser of KML, the Keyhole Markup Language, an XML format."

mkOptions :: String -> KmlOptions
mkOptions programName =
    KmlOptions
        { file = def &= help "With this one KML file"
        }
        &= summary description
        &= program programName

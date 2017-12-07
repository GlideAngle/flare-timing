{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Igc.Options (IgcOptions(..), mkOptions) where

import Text.RawString.QQ (r)
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
data IgcOptions
    = IgcOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
                 deriving (Show, Data, Typeable)

description :: String
description = [r|
A parser for IGC, a plain-text file format from the International Gliding
Commission for recording flights.
|]

mkOptions :: String -> IgcOptions
mkOptions programName =
    IgcOptions
        { dir = def &= help "Over all the IGC files in this directory"
        , file = def &= help "With this one IGC file"
        }
        &= summary description
        &= program programName


{-# LANGUAGE DeriveDataTypeable #-}

module IgcOptions (IgcOptions(..), mkOptions) where

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
newtype IgcOptions
    = IgcOptions
        { file :: FilePath
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
        { file = def &= help "With this one IGC file"
        }
        &= summary description
        &= program programName

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cmd.Options (CmdOptions(..)) where

import System.Console.CmdArgs.Implicit (Default(..), Data, Typeable)
import Flight.TaskTrack (TaskDistanceMeasure(..))

deriving instance Data TaskDistanceMeasure

instance Default TaskDistanceMeasure where
    def = TaskDistanceByAllMethods


-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 , task :: [Int]
                 -- ^ Include only these tasks.
                 , measure :: TaskDistanceMeasure
                 -- ^ Use the given measure(s).
                 , noTaskWaypoints :: Bool
                 -- ^ Exclude task waypoints
                 }
                 deriving (Data, Typeable, Show)

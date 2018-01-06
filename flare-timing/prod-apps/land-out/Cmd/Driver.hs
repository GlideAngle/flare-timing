{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Data.Maybe (fromMaybe)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)
import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Nominal(..)
    , MaskTrackFile(..)
    , compToMask
    , compToLand
    , findCompInput
    )
import Flight.Units ()
import Flight.Track.Mask (Masking(..), TrackDistance(..))
import Flight.Track.Land (Landing(..))
import Flight.Scribe (readComp, readMasking, writeLanding, readLanding)
import Flight.Score
    ( MinimumDistance(..)
    , BestDistance(..)
    , PilotDistance(..)
    , Chunk(..)
    , Chunks(..)
    )
import Flight.Score as Gap (landouts, lookahead, chunks, difficultyFraction)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Land outs counted for distance difficulty completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile = do
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile = compToLand compFile
    putStrLn $ "Reading land outs from '" ++ takeFileName maskPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    masking <- runExceptT $ readMasking maskFile

    case (compSettings, masking) of
        (Left msg, _) -> putStrLn msg
        (_, Left msg) -> putStrLn msg
        (Right cs, Right mk) -> do
            writeLanding landFile $ difficulty cs mk
            x <- runExceptT $ readLanding landFile
            print x

difficulty :: CompSettings -> Masking -> Landing
difficulty CompSettings{nominal} Masking{bestDistance, land} =
    Landing 
        { minDistance = md
        , bestDistance = bestDistance
        , landout = length <$> land
        , lookahead = ahead
        , chunks = (fromMaybe zeroChunk . fmap (Gap.chunks md'')) <$> bests
        , chunkLandings = Gap.landouts md'' <$> pss
        , sumOfDifficulty = sums
        }
    where
        md = free nominal
        md' = MkQuantity $ md
        md'' = MinimumDistance md'
        zeroChunk = Chunks [Chunk md']

        pss :: [[PilotDistance (Quantity Double [u| km |])]]
        pss =
            (fmap . fmap)
            (PilotDistance
            . MkQuantity
            . (\TrackDistance{made} -> fromMaybe 0 made)
            . snd)
            land

        bests :: [Maybe BestDistance]
        bests =
            (fmap . fmap) (BestDistance . MkQuantity) bestDistance

        ahead =
            [ flip Gap.lookahead ps <$> b
            | b <- bests
            | ps <- pss
            ]

        (sums, _) =
            unzip $
            maybe (Nothing, []) (\(sd, xs) -> (Just sd, xs)) <$>
            [ (\bd -> Gap.difficultyFraction md'' bd ps) <$> b
            | b <- bests
            | ps <- pss
            ]


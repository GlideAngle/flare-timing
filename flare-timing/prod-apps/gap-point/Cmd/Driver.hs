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
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe (readComp, readMasking, writeLanding)
import Flight.Score
    ( MinimumDistance(..)
    , BestDistance(..)
    , PilotDistance(..)
    , Difficulty(..)
    , mergeChunks
    )
import qualified Flight.Score as Gap
    (ChunkDifficulty(..), landouts, lookahead, difficulty)

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

difficulty :: CompSettings -> Masking -> Cmp.Landing
difficulty CompSettings{nominal} Masking{bestDistance, land} =
    Cmp.Landing 
        { minDistance = md''
        , bestDistance = bests
        , landout = length <$> land
        , lookahead = ahead
        , sumOfDifficulty = (fmap sumOf) <$> ds
        , difficulty = cs
        }
    where
        md = free nominal
        md' = MkQuantity $ md
        md'' = MinimumDistance md'

        pss :: [[PilotDistance (Quantity Double [u| km |])]]
        pss =
            (fmap . fmap)
            (PilotDistance
            . MkQuantity
            . (\TrackDistance{made} -> fromMaybe 0 made)
            . snd)
            land

        bests :: [Maybe (BestDistance (Quantity Double [u| km |]))]
        bests =
            (fmap . fmap) (BestDistance . MkQuantity) bestDistance

        ahead =
            [ flip Gap.lookahead ps <$> b
            | b <- bests
            | ps <- pss
            ]

        ds :: [Maybe Difficulty] =
            [ (\bd -> Gap.difficulty md'' bd ps) <$> b
            | b <- bests
            | ps <- pss
            ]

        cs :: [Maybe [Gap.ChunkDifficulty]] =
            [ do
                ils' <- ils
                jls' <- jls
                as' <- as
                jas' <- jas
                rs' <- rs
                fs' <- fs
                return $ mergeChunks ls ils' jls' as' jas' rs' fs'
            | ls <- Gap.landouts md'' <$> pss
            | ils <- (fmap . fmap) startChunk ds
            | jls <- (fmap . fmap) endChunk ds
            | as <- (fmap downward) <$> ds
            | jas <- (fmap . fmap) endAhead ds
            | rs <- (fmap relative) <$> ds
            | fs <- (fmap fractional) <$> ds
            ]

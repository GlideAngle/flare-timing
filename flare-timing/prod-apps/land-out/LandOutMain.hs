{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , CompSettings(..)
    , Nominal(..)
    , MaskEffortFile(..)
    , compToMaskEffort
    , compToLand
    , findCompInput
    , ensureExt
    )
import Flight.Distance (unTaskDistanceAsKm)
import Flight.Track.Distance (TrackDistance(..))
import Flight.Track.Mask (MaskingEffort(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe (readComp, readMaskingEffort, writeLanding)
import Flight.Score
    ( FlownMax(..)
    , PilotDistance(..)
    , Difficulty(..)
    , Pilot
    , mergeChunks
    )
import qualified Flight.Score as Gap
    (ChunkDifficulty(..), landouts, lookahead, gradeDifficulty)
import LandOutOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Land outs counted for distance difficulty completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    let maskFile@(MaskEffortFile maskPath) = compToMaskEffort compFile
    let landFile = compToLand compFile
    putStrLn $ "Reading land outs from '" ++ takeFileName maskPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readMaskingEffort maskFile)
            (const $ return Nothing)

    case (compSettings, masking) of
        (Nothing, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing) -> putStrLn "Couldn't read the maskings."
        (Just cs, Just mk) -> writeLanding landFile $ difficulty cs mk

difficulty :: CompSettings k -> MaskingEffort -> Cmp.Landing
difficulty CompSettings{nominal} MaskingEffort{bestEffort, land} =
    Cmp.Landing
        { minDistance = md
        , bestDistance = bests
        , landout = length <$> land
        , lookahead = ahead
        , sumOfDifficulty = fmap sumOf <$> es
        , difficulty = cs
        }
    where
        md = free nominal

        pss :: [[Pilot]]
        pss = (fmap . fmap) fst land

        dss :: [[PilotDistance (Quantity Double [u| km |])]]
        dss =
            (fmap . fmap)
            (PilotDistance
            . MkQuantity
            . (\TrackDistance{made} -> maybe 0 unTaskDistanceAsKm made)
            . snd)
            land

        bests :: [Maybe (FlownMax (Quantity Double [u| km |]))]
        bests =
            (fmap . fmap) (FlownMax . MkQuantity . unTaskDistanceAsKm) bestEffort

        ahead =
            [ flip Gap.lookahead ds <$> b
            | b <- bests
            | ds <- dss
            ]

        es :: [Maybe Difficulty] =
            [ (\bd -> Gap.gradeDifficulty md bd ps ds) <$> b
            | b <- bests
            | ps <- pss
            | ds <- dss
            ]

        cs :: [Maybe [Gap.ChunkDifficulty]] =
            [ do
                is' <- is
                js' <- js
                ks' <- ks
                dws' <- dws
                rs' <- rs
                mergeChunks los is' js' ks' dws' rs' <$> fs
            | los <- [Gap.landouts md ps ds | ps <- pss| ds <- dss]
            | is <- (fmap . fmap) startChunk es
            | js <- (fmap . fmap) endChunk es
            | ks <- (fmap . fmap) endAhead es
            | dws <- fmap downward <$> es
            | rs <- fmap relative <$> es
            | fs <- fmap fractional <$> es
            ]

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Zip (munzip)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , CompSettings(..)
    , Nominal(..)
    , MaskEffortFile(..)
    , compToMaskEffort
    , compToFar
    , findCompInput
    , ensureExt
    )
import Flight.Distance (unTaskDistanceAsKm)
import Flight.Track.Distance (TrackDistance(..))
import Flight.Track.Mask (MaskingEffort(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe (readComp, readMaskingEffort, writeFaring)
import "flight-gap-allot" Flight.Score
    (FlownMax(..), PilotDistance(..), MinimumDistance(..), Pilot)
import "flight-gap-effort" Flight.Score (Difficulty(..), mergeChunks)
import qualified "flight-gap-effort" Flight.Score as Gap
    (Chunking(..), ChunkDifficulty(..), landouts, lookahead, gradeDifficulty)
import FarOutOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o@CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Far outs counted for distance difficulty completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    let maskFile@(MaskEffortFile maskPath) = compToMaskEffort compFile
    let farFile = compToFar compFile
    putStrLn $ "Reading far outs from '" ++ takeFileName maskPath ++ "'"

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
        (Just cs, Just mk) -> writeFaring farFile $ difficulty cs mk

difficulty :: CompSettings k -> MaskingEffort -> Cmp.Landing
difficulty CompSettings{nominal} MaskingEffort{bestEffort, land} =
    Cmp.Landing
        { minDistance = md
        , bestDistance = bests
        , landout = length <$> land
        , lookahead = ahead
        , chunking = cgs
        , difficulty = cs
        }
    where
        md@(MinimumDistance (MkQuantity free')) = free nominal

        pss :: [[Pilot]]
        pss = (fmap . fmap) fst land

        dss :: [[PilotDistance (Quantity Double [u| km |])]]
        dss =
            (fmap . fmap)
            (PilotDistance
            . MkQuantity
            . (\TrackDistance{made} ->
                -- NOTE: Anyone not having a made distance is given the free one.
                maybe free' unTaskDistanceAsKm made)
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

        (cgs, es) :: ([Maybe Gap.Chunking], [Maybe Difficulty]) =
            unzip $
            munzip <$>
            [ (\bd -> Gap.gradeDifficulty bd ps ds) <$> b
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
            | los <- [Gap.landouts ps ds | ps <- pss| ds <- dss]
            | is <- (fmap . fmap) startChunk es
            | js <- (fmap . fmap) endChunk es
            | ks <- (fmap . fmap) endAhead es
            | dws <- fmap downward <$> es
            | rs <- fmap relative <$> es
            | fs <- fmap fractional <$> es
            ]

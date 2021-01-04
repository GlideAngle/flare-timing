{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Zip (munzip)
import Control.Exception.Safe (catchIO)
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
    , CompTaskSettings(..)
    , Nominal(..)
    , findCompInput
    , reshape
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Distance (unTaskDistanceAsKm)
import Flight.Track.Distance (TrackDistance(..))
import Flight.Track.Mask (CompMaskingEffort(..))
import qualified Flight.Track.Land as Cmp (CompLanding(..))
import Flight.Scribe (readCompAndTasks, readCompMaskEffort, writeCompLandOut)
import "flight-gap-allot" Flight.Score
    (FlownMax(..), PilotDistance(..), MinimumDistance(..), Pilot)
import "flight-gap-effort" Flight.Score (Difficulty(..), mergeChunks)
import qualified "flight-gap-effort" Flight.Score as Gap
    (Chunking(..), ChunkDifficulty(..), landouts, lookahead, gradeDifficulty)
import LandOutOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape CompInput}
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
    fprint ("Land outs counted for distance difficulty completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readCompMaskEffort compFile)
            (const $ return Nothing)

    case (filesTaskAndSettings, masking) of
        (Nothing, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing) -> putStrLn "Couldn't read the maskings."
        (Just (_taskFiles, settings), Just mk) ->
            let cs = uncurry mkCompTaskSettings $ settings
            in writeCompLandOut compFile $ difficulty cs mk

difficulty :: CompTaskSettings k -> CompMaskingEffort -> Cmp.CompLanding
difficulty CompTaskSettings{nominal} CompMaskingEffort{bestEffort, land} =
    Cmp.CompLanding
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

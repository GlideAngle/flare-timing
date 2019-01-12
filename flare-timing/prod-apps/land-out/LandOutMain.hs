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
    , MaskTrackFile(..)
    , compToMask
    , compToLand
    , findCompInput
    , ensureExt
    )
import Flight.Distance (unTaskDistanceAsKm)
import Flight.Track.Distance (TrackDistance(..))
import Flight.Track.Mask (Masking(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe (readComp, readMasking, writeLanding)
import Flight.Score
    ( BestDistance(..)
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
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile = compToLand compFile
    putStrLn $ "Reading land outs from '" ++ takeFileName maskPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readMasking maskFile)
            (const $ return Nothing)

    case (compSettings, masking) of
        (Nothing, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing) -> putStrLn "Couldn't read the maskings."
        (Just cs, Just mk) -> writeLanding landFile $ difficulty cs mk

difficulty :: CompSettings k -> Masking -> Cmp.Landing
difficulty CompSettings{nominal} Masking{bestDistance, land} =
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

        bests :: [Maybe (BestDistance (Quantity Double [u| km |]))]
        bests =
            (fmap . fmap) (BestDistance . MkQuantity . unTaskDistanceAsKm) bestDistance

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
                ils' <- ils
                jls' <- jls
                as' <- as
                jas' <- jas
                rs' <- rs
                mergeChunks ls ils' jls' as' jas' rs' <$> fs
            | ls <- Gap.landouts md <$> pss <*> dss
            | ils <- (fmap . fmap) startChunk es
            | jls <- (fmap . fmap) endChunk es
            | as <- fmap downward <$> es
            | jas <- (fmap . fmap) endAhead es
            | rs <- fmap relative <$> es
            | fs <- fmap fractional <$> es
            ]

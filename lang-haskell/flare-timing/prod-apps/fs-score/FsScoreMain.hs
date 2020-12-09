import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Control.Arrow (second)
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseNominal, parseNormScores)
import Flight.Track.Speed (TrackSpeed)
import qualified Flight.Track.Speed as Time (TrackSpeed(..))
import Flight.Track.Point (NormPointing(..), NormBreakdown(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(TrimFsdb)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , Pilot(..)
    , Nominal(..)
    , trimFsdbToNormScore
    , findTrimFsdb
    , reshape
    )
import qualified "flight-gap-allot" Flight.Score as Gap (bestTime')
import qualified "flight-gap-allot" Flight.Score as Frac (Fractions(..))
import "flight-gap-allot" Flight.Score
    ( BestTime(..), PilotTime(..)
    , LeadingFraction(..)
    , ArrivalFraction(..)
    , DistanceFraction(..)
    , LinearFraction(..)
    , DifficultyFraction(..)
    , speedFraction
    )
import "flight-gap-math" Flight.Score
    ( Points(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    )
import Flight.Scribe (readTrimFsdb, writeNormScore)
import FsScoreOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape TrimFsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findTrimFsdb $ FindDirFile {dir = cwd, file = file}

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative scores completed in " % timeSpecs % "\n") start end

go :: TrimFsdbFile -> IO ()
go trimFsdbFile = do
    FsdbXml contents <- readTrimFsdb trimFsdbFile
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ normScores (FsdbXml contents')
    either print (writeNormScore (trimFsdbToNormScore trimFsdbFile)) settings

fsdbNominal :: FsdbXml -> ExceptT String IO Nominal
fsdbNominal (FsdbXml contents) = do
    ns <- lift $ parseNominal contents
    case ns of
        Left msg -> ExceptT . return $ Left msg
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbScores :: Nominal -> FsdbXml -> ExceptT String IO NormPointing
fsdbScores n (FsdbXml contents) = do
    fs <- lift $ parseNormScores n contents
    ExceptT $ return fs

normScores :: FsdbXml -> ExceptT String IO NormPointing
normScores fsdbXml = do
    n <- fsdbNominal fsdbXml
    np@NormPointing{score = xss} <- fsdbScores n fsdbXml

    let vss :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
            times <$> xss

    let tss =
            [
                reverse . sortOn (total . snd) $
                maybe
                    xs
                    (\vs' ->
                        let vMap = Map.fromList vs' in
                        [
                            case Map.lookup p vMap of
                                Nothing -> px
                                Just
                                    Time.TrackSpeed
                                        { Time.time = tt
                                        , Time.frac = tf
                                        } -> (p, x{timeElapsed = Just tt, fractions = fracs{Frac.time = tf}})
                        | px@(p, x@NormBreakdown{fractions = fracs}) <- xs
                        ])
                    vs

            | xs <- xss
            | vs <- (fmap . fmap) snd vss
            ]

    let lss =
            [
                maybe
                    xs
                    (\ys ->
                        [ (p, t{fractions = fracs{Frac.leading = c}})
                        | (_, c) <- ys
                        | (p, t@NormBreakdown{fractions = fracs}) <- xs
                        ]
                    )
                    (leads xs)

            | xs <- tss
            ]

    let ass =
            [
                maybe
                    xs
                    (\ys ->
                        [ (p, t{fractions = fracs{Frac.arrival = c}})
                        | (_, c) <- ys
                        | (p, t@NormBreakdown{fractions = fracs}) <- xs
                        ]
                    )
                    (arrivals xs)

            | xs <- lss
            ]

    let rss =
            [
                maybe
                    xs
                    (\ys ->
                        [ (p, t{fractions = fracs{Frac.reach = c}})
                        | (_, c) <- ys
                        | (p, t@NormBreakdown{fractions = fracs}) <- xs
                        ]
                    )
                    (reaches xs)

            | xs <- ass
            ]

    let ess =
            [
                maybe
                    xs
                    (\ys ->
                        [ (p, t{fractions = fracs{Frac.effort = c}})
                        | (_, c) <- ys
                        | (p, t@NormBreakdown{fractions = fracs}) <- xs
                        ]
                    )
                    (efforts xs)

            | xs <- rss
            ]

    let dss =
            [
                maybe
                    xs
                    (\ys ->
                        [ (p, t{fractions = fracs{Frac.distance = c}})
                        | (_, c) <- ys
                        | (p, t@NormBreakdown{fractions = fracs}) <- xs
                        ]
                    )
                    (distances xs)

            | xs <- ess
            ]

    return np{bestTime = (fmap . fmap) fst vss, score = dss}

arrivals :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, ArrivalFraction)]
arrivals xs =
    (\ lf -> second (g lf) <$> ys)
    <$> maxArrivalPoints cs
    where
        ys :: [(Pilot, ArrivalPoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{arrival = c}}) -> (p,c)) <$> xs

        cs :: [ArrivalPoints]
        cs = snd <$> ys

        g arMax ar = arrivalFraction arMax ar

reachFraction :: LinearPoints -> LinearPoints -> LinearFraction
reachFraction (LinearPoints maxPts) (LinearPoints pts)
    | maxPts == 0 = LinearFraction 0
    | otherwise = LinearFraction $ pts / maxPts

effortFraction :: DifficultyPoints -> DifficultyPoints -> DifficultyFraction
effortFraction (DifficultyPoints maxPts) (DifficultyPoints pts)
    | maxPts == 0 = DifficultyFraction 0
    | otherwise = DifficultyFraction . realToFrac $ pts / maxPts

distanceFraction :: DistancePoints -> DistancePoints -> DistanceFraction
distanceFraction (DistancePoints maxPts) (DistancePoints pts)
    | maxPts == 0 = DistanceFraction 0
    | otherwise = DistanceFraction $ pts / maxPts

arrivalFraction :: ArrivalPoints -> ArrivalPoints -> ArrivalFraction
arrivalFraction (ArrivalPoints maxPts) (ArrivalPoints pts)
    | maxPts == 0 = ArrivalFraction 0
    | otherwise = ArrivalFraction $ pts / maxPts

leadingFraction :: LeadingPoints -> LeadingPoints -> LeadingFraction
leadingFraction (LeadingPoints maxPts) (LeadingPoints pts)
    | maxPts == 0 = LeadingFraction 0
    | otherwise = LeadingFraction $ pts / maxPts

maxLinearPoints :: [LinearPoints] -> Maybe LinearPoints
maxLinearPoints [] = Nothing
maxLinearPoints xs = Just $ maximum xs

maxDifficultyPoints :: [DifficultyPoints] -> Maybe DifficultyPoints
maxDifficultyPoints [] = Nothing
maxDifficultyPoints xs = Just $ maximum xs

maxDistancePoints :: [DistancePoints] -> Maybe DistancePoints
maxDistancePoints [] = Nothing
maxDistancePoints xs = Just $ maximum xs

maxArrivalPoints :: [ArrivalPoints] -> Maybe ArrivalPoints
maxArrivalPoints [] = Nothing
maxArrivalPoints xs = Just $ maximum xs

maxLeadingPoints :: [LeadingPoints] -> Maybe LeadingPoints
maxLeadingPoints [] = Nothing
maxLeadingPoints xs = Just $ maximum xs

reaches :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, LinearFraction)]
reaches xs =
    (\lf -> second (g lf) <$> ys) <$> maxLinearPoints cs
    where
        ys :: [(Pilot, LinearPoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{reach = c}}) -> (p,c)) <$> xs

        cs :: [LinearPoints]
        cs = snd <$> ys

        g rMin r = reachFraction rMin r

efforts :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, DifficultyFraction)]
efforts xs =
    (\lf -> second (g lf) <$> ys) <$> maxDifficultyPoints cs
    where
        ys :: [(Pilot, DifficultyPoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{effort = c}}) -> (p,c)) <$> xs

        cs :: [DifficultyPoints]
        cs = snd <$> ys

        g eMin e = effortFraction eMin e

distances :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, DistanceFraction)]
distances xs =
    (\lf -> second (g lf) <$> ys) <$> maxDistancePoints cs
    where
        ys :: [(Pilot, DistancePoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{distance = c}}) -> (p,c)) <$> xs

        cs :: [DistancePoints]
        cs = snd <$> ys

        g dMin d = distanceFraction dMin d

leads :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, LeadingFraction)]
leads xs =
    (\lf -> second (g lf) <$> ys) <$> maxLeadingPoints cs
    where
        ys :: [(Pilot, LeadingPoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{leading = c}}) -> (p,c)) <$> xs

        cs :: [LeadingPoints]
        cs = snd <$> ys

        g lcMin lc = leadingFraction lcMin lc

times
    :: [(Pilot, NormBreakdown)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times xs =
    (\bt -> (bt, second (g bt) <$> ys)) <$> Gap.bestTime' ts
    where
        ys :: [(Pilot, PilotTime (Quantity Double [u| h |]))]
        ys =
            catMaybes
            $ (\(p, NormBreakdown{timeElapsed = t}) -> (p,) <$> t)
            <$> xs

        ts :: [PilotTime (Quantity Double [u| h |])]
        ts = snd <$> ys

        g best t =
            Time.TrackSpeed
                { Time.time = t
                , Time.frac = speedFraction best t
                }

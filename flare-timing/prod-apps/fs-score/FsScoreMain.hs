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

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseNominal, parseNormScores)
import Flight.Track.Speed (TrackSpeed)
import qualified Flight.Track.Speed as Time (TrackSpeed(..))
import Flight.Track.Point (NormPointing(..), NormBreakdown(..))
import Flight.Comp
    ( FileType(Fsdb)
    , FsdbFile(..)
    , FsdbXml(..)
    , Pilot(..)
    , Nominal(..)
    , fsdbToNormScore
    , findFsdb
    , ensureExt
    )
import qualified Flight.Score as Gap (bestTime')
import Flight.Score
    ( BestTime(..), PilotTime(..)
    , LeadingFraction(..), LeadingPoints(..)
    , ArrivalFraction(..), ArrivalPoints(..)
    , Points(..)
    , speedFraction
    )
import Flight.Scribe (writeNormScore)
import FsScoreOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt Fsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findFsdb o

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative scores completed in " % timeSpecs % "\n") start end

go :: FsdbFile -> IO ()
go fsdbFile@(FsdbFile fsdbPath) = do
    contents <- readFile fsdbPath
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ normScores (FsdbXml contents')
    either print (writeNormScore (fsdbToNormScore fsdbFile)) settings

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
                                        } -> (p, x{timeElapsed = Just tt, timeFrac = tf})
                        | px@(p, x) <- xs
                        ])
                    vs

            | xs <- xss
            | vs <- (fmap . fmap) snd vss
            ]

    let lss =
            [
                maybe
                    ts
                    (\ls ->
                        [ (p, t{leadingFrac = c})
                        | (_, c) <- ls
                        | (p, t) <- ts
                        ]
                    )
                    (leads ts)

            | ts <- tss
            ]

    let ass =
            [
                maybe
                    ls
                    (\as ->
                        [ (p, t{arrivalFrac = c})
                        | (_, c) <- as
                        | (p, t) <- ls
                        ]
                    )
                    (arrivals ls)

            | ls <- lss
            ]

    return np{bestTime = (fmap . fmap) fst vss, score = ass}

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

arrivalFraction :: ArrivalPoints -> ArrivalPoints -> ArrivalFraction
arrivalFraction (ArrivalPoints maxPts) (ArrivalPoints pts)
    | maxPts == 0 = ArrivalFraction 0
    | otherwise = ArrivalFraction . toRational $ pts / maxPts

maxArrivalPoints :: [ArrivalPoints] -> Maybe ArrivalPoints
maxArrivalPoints [] = Nothing
maxArrivalPoints xs = Just $ maximum xs

leads :: [(Pilot, NormBreakdown)] -> Maybe [(Pilot, LeadingFraction)]
leads xs =
    (\ lf -> second (g lf) <$> ys)
    <$> maxLeadingPoints cs
    where
        ys :: [(Pilot, LeadingPoints)]
        ys = (\(p, NormBreakdown{breakdown = Points{leading = c}}) -> (p,c)) <$> xs

        cs :: [LeadingPoints]
        cs = snd <$> ys

        g lcMin lc = leadingFraction lcMin lc

leadingFraction :: LeadingPoints -> LeadingPoints -> LeadingFraction
leadingFraction (LeadingPoints maxPts) (LeadingPoints pts)
    | maxPts == 0 = LeadingFraction 0
    | otherwise = LeadingFraction . toRational $ pts / maxPts

maxLeadingPoints :: [LeadingPoints] -> Maybe LeadingPoints
maxLeadingPoints [] = Nothing
maxLeadingPoints xs = Just $ maximum xs

times
    :: [(Pilot, NormBreakdown)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times xs =
    (\ bt -> (bt, second (g bt) <$> ys))
    <$> Gap.bestTime' ts
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

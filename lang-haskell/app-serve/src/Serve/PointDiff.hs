module Serve.PointDiff (DiffWay(..), getTaskPointsDiffStats) where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.Vector (Vector)
import Servant (throwError)

import qualified Flight.Track.Point as Alt (AltPointing(..), AltBreakdown(..))
import Flight.Track.Point (Pointing(..), Breakdown(..))
import "flight-gap-math" Flight.Score (TaskPoints(..))
import Flight.Comp (Pilot(..))
import Serve.Config (AppT(..), Config(pointing, altFsScore, altAsScore))
import Serve.Error (errTaskPoints, errAltPoints)

data DiffWay = DiffWayFtFs | DiffWayFtAs | DiffWayAsFs

getTaskPointsDiffStats :: DiffWay -> AppT k IO [Maybe (Double, Double)]

getTaskPointsDiffStats DiffWayFtFs = do
    ys <- fmap (\Pointing{score} -> (fmap . fmap . fmap) (\Breakdown{total} -> total) score) <$> asks pointing
    xs <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altFsScore
    getDiffStats ys xs

getTaskPointsDiffStats DiffWayFtAs = do
    ys <- fmap (\Pointing{score} -> (fmap . fmap . fmap) (\Breakdown{total} -> total) score) <$> asks pointing
    xs <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altAsScore
    getDiffStats ys xs

getTaskPointsDiffStats DiffWayAsFs = do
    ys <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altAsScore
    xs <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altFsScore
    getDiffStats ys xs

getDiffStats
    :: Maybe [[(Pilot, TaskPoints)]]
    -> Maybe [[(Pilot, TaskPoints)]]
    -> AppT k IO [Maybe (Double, Double)]
getDiffStats ys xs = do
    case (ys, xs) of
        (Just ys', Just xs') -> do
            let taskDiffs =
                    [
                        let xsMap = Map.fromList xsTask in
                        [(Map.lookup pilot xsMap, p') | (pilot, p') <- ysTask]

                    | ysTask <- ys'
                    | xsTask <- xs'
                    ]

            return $ (uncurry diffStats . unzip) <$> taskDiffs

        (Nothing, _) -> throwError errTaskPoints
        (_, Nothing) -> throwError errAltPoints

diffStats :: [Maybe TaskPoints] -> [TaskPoints] -> Maybe (Double, Double)
diffStats xs ys =
    let xs' :: Maybe [TaskPoints]
        xs' = sequence xs
    in do
            xs'' <- xs'
            let diffs = zipWith (\(TaskPoints y) (TaskPoints x) -> y - x) ys xs''
            (mean, variance) <- maybeMeanVariance $ V.fromList diffs
            return (mean, sqrt variance)

maybeMeanVariance :: Vector Double -> Maybe (Double, Double)
maybeMeanVariance xs = if null xs then Nothing else Just $ Stats.meanVariance xs

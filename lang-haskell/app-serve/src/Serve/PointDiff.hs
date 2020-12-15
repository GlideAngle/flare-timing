module Serve.PointDiff (getTaskPointsDiffStats) where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.Vector (Vector)
import Servant (throwError)

import qualified Flight.Track.Point as Alt (AltPointing(..), AltBreakdown(..))
import Flight.Track.Point (Pointing(..), Breakdown(..))
import "flight-gap-math" Flight.Score (TaskPoints(..))
import Serve.Config (AppT(..), Config(pointing, altFsScore))
import Serve.Error (errTaskPoints, errAltPoints)

getTaskPointsDiffStats :: AppT k IO [Maybe (Double, Double)]
getTaskPointsDiffStats = do
    ps <- fmap (\Pointing{score} -> (fmap . fmap . fmap) (\Breakdown{total} -> total) score) <$> asks pointing
    exs <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altFsScore
    case (ps, exs) of
        (Just ps', Just exs') -> do
            let taskDiffs =
                    [
                        let exsMap = Map.fromList exsTask in
                        [(Map.lookup pilot exsMap, p') | (pilot, p') <- psTask]

                    | psTask <- ps'
                    | exsTask <- exs'
                    ]

            return $ (uncurry diffStats . unzip) <$> taskDiffs

        (Nothing, _) -> throwError errTaskPoints
        (_, Nothing) -> throwError errAltPoints

diffStats :: [Maybe TaskPoints] -> [TaskPoints] -> Maybe (Double, Double)
diffStats es ps =
    let es' :: Maybe [TaskPoints]
        es' = sequence es
     in do
            es'' <- es'
            let xs = zipWith (\(TaskPoints p) (TaskPoints e) -> p - e) ps es''
            (mean, variance) <- maybeMeanVariance $ V.fromList xs
            return (mean, sqrt variance)

maybeMeanVariance :: Vector Double -> Maybe (Double, Double)
maybeMeanVariance xs = if null xs then Nothing else Just $ Stats.meanVariance xs

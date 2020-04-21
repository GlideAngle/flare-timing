module FlareTiming.Plot.Reach.View (reachPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Data.Maybe (isJust)
import Data.List (sortOn)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (reachPlot)

import WireTypes.Fraction (ReachFraction(..))
import WireTypes.Comp (Task(..))
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Plot.Reach.TableReach (tablePilotReach)
import FlareTiming.Plot.Reach.TableBonus (tablePilotReachBonus)

placings :: [TrackReach] -> [[Double]]
placings = fmap xy

xy :: TrackReach -> [Double]
xy TrackReach{reach = PilotDistance x, frac = ReachFraction y} =
    [x, y]

rawReach :: TrackReach -> Double
rawReach TrackReach{reach = PilotDistance x} = x

timeRange :: [TrackReach] -> (Double, Double)
timeRange xs = let rXs = rawReach <$> xs in (minimum rXs, maximum rXs)

reValue :: [(Pilot, TrackReach)] -> [(Pilot, TrackReach)] -> [[Double]]
reValue pxs pys =
    [ [x, y]
    | (_, TrackReach{reach = PilotDistance x}) <- sortOn fst pxs
    | (_, TrackReach{frac = ReachFraction y}) <- sortOn fst pys
    ]

reachPlot
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
reachPlot task sEx reach bonusReach = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-reach") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let xs = snd . unzip $ reach'
                            let tt = timeRange xs
                            _ <-
                                if isJust stopped then
                                    P.reachPlot
                                        (_element_raw elPlot)
                                        tt
                                        (placings xs)
                                        (reValue reach' bonusReach')
                                else
                                    P.reachPlot
                                        (_element_raw elPlot)
                                        tt
                                        (placings xs)
                                        []

                            return ())
                        ]

                    reach' <- sample . current $ reach
                    Task{stopped} <- sample . current $ task
                    bonusReach' <- sample . current $ bonusReach

                return ()

        elClass "div" "tile is-child" $ do
            _ <- dyn $ ffor task (\case
                    Task{stopped = Nothing} -> tablePilotReach sEx reach
                    Task{stopped = Just _} -> tablePilotReachBonus sEx reach bonusReach)

            return ()

    return ()

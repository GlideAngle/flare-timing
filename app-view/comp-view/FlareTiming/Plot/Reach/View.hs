module FlareTiming.Plot.Reach.View (reachPlot) where

import Data.Maybe (isJust)
import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (reachPlot)

import WireTypes.Comp (Task(..))
import WireTypes.Reach (TrackReach(..), ReachFraction(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import FlareTiming.Pilot (showPilotName)

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
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
reachPlot task reach bonusReach = do
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
                    Task{stopped = Nothing} -> tablePilotReach reach
                    Task{stopped = Just _} -> tablePilotReachBonus reach bonusReach)

            return ()

    return ()

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReach reach = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Reach (km)"
                    elClass "th" "th-plot-frac" $ text "Fraction"
                    el "th" $ text "Pilot"

                    return ()

            _ <- el "tbody" $
                    simpleList reach (uncurry rowReach . splitDynPure)

            return ()
    return ()

rowReach
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReach p r = do
    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r
        elClass "td" "td-plot-frac" . dynText $ showFrac . frac <$> r
        elClass "td" "td-pilot" . dynText $ showPilotName <$> p

        return ()

tablePilotReachBonus
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReachBonus reach bonusReach = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3") $ text "Reach (km)"
                    elAttr "th" ("colspan" =: "2") $ text "Fraction"
                    el "th" $ text ""

                    return ()

            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Flown"
                    elClass "th" "th-plot-reach-bonus" $ text "Scored"
                    elClass "th" "th-plot-reach-bonus-diff" $ text "Δ"
                    elClass "th" "th-plot-frac" $ text "Flown"
                    elClass "th" "th-plot-frac-bonus" $ text "Scored"
                    el "th" $ text "Pilot"

                    return ()

            _ <- dyn $ ffor bonusReach (\br -> do
                    let mapR = Map.fromList br

                    el "tbody" $
                        simpleList reach (uncurry (rowReachBonus mapR) . splitDynPure))

            return ()
    return ()

rowReachBonus
    :: MonadWidget t m
    => Map Pilot TrackReach
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReachBonus mapR p r = do
    (bReach, diffReach, bFrac) <- sample . current
            $ ffor2 p r (\p' r' ->
                case Map.lookup p' mapR of
                    Just br ->
                        let PilotDistance rBonus = reach $ br
                            PilotDistance rFlown = reach $ r'
                            diffReach = rBonus - rFlown
                        in
                            ( (showPilotDistance 1) $ reach br
                            , T.pack $ printf "%+.1f" diffReach
                            , showFrac . frac $ br
                            )

                    _ -> ("", "", ""))

    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r
        elClass "td" "td-plot-reach-bonus" $ text bReach
        elClass "td" "td-plot-reach-bonus-diff" $ text diffReach

        elClass "td" "td-plot-frac" . dynText $ showFrac . frac <$> r
        elClass "td" "td-plot-frac-bonus" $ text bFrac

        elClass "td" "td-pilot" . dynText $ showPilotName <$> p

        return ()

showFrac :: ReachFraction -> T.Text
showFrac (ReachFraction x) = T.pack $ printf "%.3f" x

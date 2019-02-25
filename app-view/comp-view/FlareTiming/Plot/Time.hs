module FlareTiming.Plot.Time (timePlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Speed (TrackSpeed(..))
import FlareTiming.Plot.Time.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

timePlot
    :: MonadWidget t m
    => Dynamic t (Maybe [(Pilot, TrackSpeed)])
    -> m ()
timePlot tm = do
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $ do
                    _ <- dyn $ ffor tm (\case
                            Nothing ->
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "Time"
                                    el "p" $ text "Loading times ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Time"
                                    el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals and no time."

                            _ ->
                                elClass "article" "tile is-child box" $
                                    hgPlot (fromMaybe [] <$> tm))

                    return ()

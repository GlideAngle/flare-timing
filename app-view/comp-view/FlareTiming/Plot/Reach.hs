module FlareTiming.Plot.Reach (reachPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Reach (TrackReach(..))
import FlareTiming.Plot.Reach.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

reachPlot
    :: MonadWidget t m
    => Dynamic t (Maybe [(Pilot, TrackReach)])
    -> m ()
reachPlot rh =
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $ do
                    _ <- dyn $ ffor rh (\case
                            Nothing ->
                                elClass "article" "tile is-child" $ do
                                    elClass "p" "title" $ text "Time"
                                    el "p" $ text "Loading reach ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Reach"
                                    el "p" $ text "No pilots started. There are no distances reached."

                            _ ->
                                elClass "article" "tile is-child" $
                                    hgPlot (fromMaybe [] <$> rh))

                    return ()

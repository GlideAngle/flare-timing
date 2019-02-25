module FlareTiming.Plot.Time (timePlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Lead (TrackLead(..))
import WireTypes.Speed (TrackSpeed(..))
import WireTypes.Comp (Discipline(..))
import FlareTiming.Plot.Time.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

timePlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe [(Pilot, TrackLead)])
    -> Dynamic t (Maybe [(Pilot, TrackSpeed)])
    -> m ()
timePlot hgOrPg ld _ = do
    elClass "div" "tile is-ancestor" $ do
        _ <- dyn $ ffor hgOrPg (\hgOrPg' ->
            if hgOrPg' /= HangGliding then return () else
                elClass "div" "tile is-12" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent" $ do
                            _ <- dyn $ ffor ld (\case
                                    Nothing ->
                                        elClass "article" "tile is-child box" $ do
                                            elClass "p" "title" $ text "Leading"
                                            el "p" $ text "Loading arrivals ..."

                                    Just [] ->
                                        elClass "article" "tile is-child notification is-warning" $ do
                                            elClass "p" "title" $ text "Leading"
                                            el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals"

                                    _ ->
                                        elClass "article" "tile is-child box" $
                                            hgPlot (fromMaybe [] <$> ld))

                            return ())

        return ()

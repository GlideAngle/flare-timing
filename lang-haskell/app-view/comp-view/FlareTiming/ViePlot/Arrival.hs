module FlareTiming.ViePlot.Arrival (arrivalViePlot) where

import Reflex.Dom
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)

import WireTypes.Arrival (TrackArrival(..))
import WireTypes.Comp (Discipline(..), Tweak(..))
import qualified FlareTiming.Plot.ArrivalPosition.View as V (arrivalPositionPlot)
import qualified FlareTiming.Plot.ArrivalTime.View as V (arrivalTimePlot)
import WireTypes.Pilot (Pilot(..))

noPg :: T.Text
noPg = "Arrival place is not scored in paragliding."

noArrivals :: T.Text
noArrivals = "No pilots made it to the end of the speed section. There are no arrivals"

arrivalViePlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe [(Pilot, TrackArrival)])
    -> Dynamic t (Maybe [(Pilot, TrackArrival)])
    -> m ()
arrivalViePlot hgOrPg tweak av avN = do
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    _ <- dyn $ ffor av (\case
                            Nothing ->
                                elClass "article" "tile is-child" $ do
                                    elClass "p" "title" $ text "Arrivals"
                                    el "p" $ text "Loading arrivals ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Arrive"

                                    _ <- dyn $ ffor hgOrPg (\hgOrPg' ->
                                            if hgOrPg' /= HangGliding
                                               then
                                                    el "p" . text $ noPg <> " " <> noArrivals
                                               else
                                                    el "p" $ text noArrivals)

                                    return ()

                            _ -> do
                                _ <- dyn $ ffor hgOrPg (\hgOrPg' ->
                                    if hgOrPg' /= HangGliding
                                       then
                                            elClass "article" "tile is-child notification is-warning" $ do
                                                elClass "p" "title" $ text "Arrive"
                                                el "p" . text $ noPg
                                       else
                                            return ())

                                let notice =
                                        elClass "article" "notification is-warning" $
                                            el "p" $ text "No points will be awarded for arrival order."

                                _ <- elClass "article" "tile is-child" $
                                    dyn $ ffor tweak (\case
                                        Just Tweak{arrivalRank = False, arrivalTime = False} ->
                                            notice

                                        Just Tweak{arrivalTime = True, arrivalRank = False} ->
                                            V.arrivalTimePlot
                                                (fromMaybe [] <$> av)
                                                (fromMaybe [] <$> avN)

                                        Just Tweak{arrivalTime = False, arrivalRank = True} ->
                                            V.arrivalPositionPlot
                                                (fromMaybe [] <$> av)
                                                (fromMaybe [] <$> avN)

                                        _ ->
                                            V.arrivalPositionPlot
                                                (fromMaybe [] <$> av)
                                                (fromMaybe [] <$> avN))

                                return ())

                    return ()

module FlareTiming.Plot.Arrival (arrivalPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Arrival (TrackArrival(..))
import WireTypes.Comp (Discipline(..))
import FlareTiming.Plot.Arrival.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

noPg :: T.Text
noPg = "Arrival place is not scored in paragliding."

noArrivals :: T.Text
noArrivals = "No pilots made it to the end of the speed section. There are no arrivals"

arrivalPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe [(Pilot, TrackArrival)])
    -> m ()
arrivalPlot hgOrPg av = do
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

                                elClass "article" "tile is-child" $
                                    hgPlot (fromMaybe [] <$> av)

                                return ())

                    return ()

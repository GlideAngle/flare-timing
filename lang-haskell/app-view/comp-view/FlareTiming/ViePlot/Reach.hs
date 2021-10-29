module FlareTiming.ViePlot.Reach (reachViePlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Comp (Task(..))
import WireTypes.Reach (TrackReach(..))
import qualified FlareTiming.ViePlot.Reach.View as V (reachViePlot)
import WireTypes.Pilot (Pilot(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))

reachViePlot
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> m ()
reachViePlot task sEx reach' bonusReach' =
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $ do
                    _ <- dyn $ ffor2 reach' bonusReach' (\reach bonusReach ->
                            case (reach, bonusReach) of
                                (Nothing, _) ->
                                    elClass "article" "tile is-child" $ do
                                        elClass "p" "title" $ text "Time"
                                        el "p" $ text "Loading reach ..."

                                (_, Nothing) ->
                                    elClass "article" "tile is-child" $ do
                                        elClass "p" "title" $ text "Time"
                                        el "p" $ text "Loading bonus reach ..."

                                (Just [], _) ->
                                    elClass "article" "tile is-child notification is-warning" $ do
                                        elClass "p" "title" $ text "Reach"
                                        el "p" $ text "No pilots started. There are no distances reached."

                                (_, Just []) ->
                                    elClass "article" "tile is-child notification is-warning" $ do
                                        elClass "p" "title" $ text "Reach"
                                        el "p" $ text "No pilots started. There are no distances reached."

                                _ ->
                                    elClass "article" "tile is-child" $
                                        V.reachViePlot
                                            task
                                            sEx
                                            (fromMaybe [] <$> reach')
                                            (fromMaybe [] <$> bonusReach'))

                    return ()

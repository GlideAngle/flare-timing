module FlareTiming.Task.Absent (tableAbsent) where

import Reflex.Dom

import WireTypes.Comp (Task(..), getAbsent)
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (rowPilot)

tableAbsent
    :: MonadWidget t m
    => Dynamic t [Pilot]
    -> Dynamic t [Pilot]
    -> Dynamic t Task
    -> m ()
tableAbsent nyp dnf task = do
    let xs = getAbsent <$> task

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "ABS"
                elClass "p" "subtitle is-6" $ text "Pilots absent from this task"
                elClass "div" "content" $ do
                    _ <- elClass "table" "table" $ do
                            el "thead" $ do
                                el "tr" $ do
                                    el "th" $ text "Id"
                                    el "th" $ text "Name"

                                simpleList xs rowPilot

                    el "p" . text
                        $ "A pilot's absence does not reduce the task validity."

                    return ()

        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "DNF"
                elClass "p" "subtitle is-6" $ text "Pilots not flying this task"
                elClass "div" "content" $ do
                    _ <- elClass "table" "table" $ do
                            el "thead" $ do
                                el "tr" $ do
                                    el "th" $ text "Id"
                                    el "th" $ text "Name"

                                simpleList dnf rowPilot

                    el "p" . text
                        $ "Both launch validity and task validity are reduced when pilots elect not to fly. The reduction is made by taking the fraction of those not flying over those present at launch."

                    return ()

        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "NYP"
                elClass "p" "subtitle is-6" $ text "Pilots not yet processed"
                elClass "div" "content" $ do
                    _ <- elClass "table" "table" $ do
                            el "thead" $ do
                                el "tr" $ do
                                    el "th" $ text "Id"
                                    el "th" $ text "Name"

                                simpleList nyp rowPilot

                    el "p" . text
                        $ "Unlike DNF pilots, these pilots do not decrease launch validity. When a task is not at full distance validity, if any one of the NYP pilots flew further then the task validity will increase when they are processed. Likewise for time validity and the fastest pilots being NYP."

                    return ()

    return ()

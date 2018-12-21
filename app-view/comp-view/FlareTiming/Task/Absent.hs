module FlareTiming.Task.Absent (tableAbsent) where

import Reflex.Dom

import WireTypes.Comp (Task(..), getAbsent)
import FlareTiming.Comms (getTaskPilotDnf)
import FlareTiming.Pilot (rowPilot)
import FlareTiming.Events (IxTask(..))

tableAbsent
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Task
    -> m ()
tableAbsent ix task = do
    let xs = getAbsent <$> task
    ys <- getTaskPilotDnf ix

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

                                simpleList ys rowPilot

                    el "p" . text
                        $ "Both launch validity and task validity are reduced when pilots elect not to fly. The reduction is made by taking the fraction of those not flying over those present at launch."

                    return ()

    return ()

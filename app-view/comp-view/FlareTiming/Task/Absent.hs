module FlareTiming.Task.Absent (tableAbsent) where

import Prelude hiding (abs)
import Reflex.Dom

import WireTypes.Pilot (Nyp(..), Dnf(..))
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms (getTaskPilotAbs, getTaskPilotDfNoTrack)
import FlareTiming.Pilot (rowPilot)

tableAbsent
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Nyp
    -> Dynamic t Dnf
    -> m ()
tableAbsent ix nyp' dnf' = do
    pb <- getPostBuild
    let nyp = unNyp <$> nyp'
    let dnf = unDnf <$> dnf'
    abs <- holdDyn [] =<< getTaskPilotAbs ix pb
    dfNoTrack <- holdDyn [] =<< getTaskPilotDfNoTrack ix pb

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "ABS"
                elClass "div" "content" $ do
                    _ <- elClass "table" "table" $ do
                            el "thead" $ do
                                el "tr" $ do
                                    el "th" $ text "Id"
                                    el "th" $ text "Name"

                                simpleList abs rowPilot

                    el "p" . text
                        $ "A pilot's absence does not reduce the task validity."

                    return ()

        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "DNF"
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
                elClass "p" "title" $ text "DF no track"
                elClass "div" "content" $ do
                    _ <- elClass "table" "table" $ do
                            el "thead" $ do
                                el "tr" $ do
                                    el "th" $ text "Id"
                                    el "th" $ text "Name"

                                simpleList dfNoTrack rowPilot

                    el "p" . text
                        $ "These pilots get awarded minimum distance."

                    return ()

        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "NYP"
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

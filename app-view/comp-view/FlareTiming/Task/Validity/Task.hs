module FlareTiming.Task.Validity.Task
    ( viewTask
    , taskWorking
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showTaskValidity, showTaskValidityDiff
    , showLaunchValidity, showLaunchValidityDiff
    , showDistanceValidity, showDistanceValidityDiff
    , showTimeValidity, showTimeValidityDiff
    , showStopValidity, showStopValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    )
import FlareTiming.Task.Validity.Widget (katexNewLine, elV, elN, elD)

taskWorking :: Vy.Validity -> T.Text
taskWorking v =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "validity &= lv * dv * tv"
    <> (maybe "" (const " * sv") (Vy.stop v))
    <> katexNewLine
    <> " &= "
    <> (Vy.showLaunchValidity . Vy.launch $ v)
    <> " * "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " * "
    <> (Vy.showTimeValidity . Vy.time $ v)
    <> (maybe "" (\sv -> " * " <> (Vy.showStopValidity $ Just sv)) (Vy.stop v))
    <> katexNewLine
    <> " &= "
    <> (Vy.showTaskValidity . Vy.task $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('task-working')"
    <> ", {throwOnError: false});"

viewTask
    :: DomBuilder t m
    => Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> m ()
viewTask
    Vy.Validity{task = dq, launch = lv, distance = dv, time = tv, stop = sv}
    Vy.Validity{task = dqN, launch = lvN, distance = dvN, time = tvN, stop = svN}
    ValidityWorking{launch = LaunchValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "p" "level subtitle is-6" $ do
                elClass "span" "level-item level-left" $
                    elClass "h2" "title is-4" . text
                        $ "Task Validity* = " <> Vy.showTaskValidity dq

                elClass "span" "level-item level-right" $
                    text "* Day Quality"

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text ""
                        elClass "th" "validity" $ text "Validity"
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text "lv"
                        el "td" $ text "Launch"
                        elV $ Vy.showLaunchValidity lv
                        elN $ Vy.showLaunchValidity lvN
                        elD $ Vy.showLaunchValidityDiff lvN lv
                        return ()

                    el "tr" $ do
                        el "td" $ text "dv"
                        el "td" $ text "Distance"
                        elV $ Vy.showDistanceValidity dv
                        elN $ Vy.showDistanceValidity dvN
                        elD $ Vy.showDistanceValidityDiff dvN dv
                        return ()

                    el "tr" $ do
                        el "td" $ text "tv"
                        el "td" $ text "Time"
                        elV $ Vy.showTimeValidity tv
                        elN $ Vy.showTimeValidity tvN
                        elD $ Vy.showTimeValidityDiff tvN tv
                        return ()

                    el "tr" $ do
                        el "td" $ text "sv"
                        el "td" $ text "Stop"
                        elV $ Vy.showStopValidity sv
                        elN $ Vy.showStopValidity svN
                        elD $ Vy.showStopValidityDiff svN sv
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Task"
                        elV $ Vy.showTaskValidity dq
                        elN $ Vy.showTaskValidity dqN
                        elD $ Vy.showTaskValidityDiff dqN dq
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "4")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ do
                    foot "✓ An expected value as calculated by the official scoring program, FS."
                    foot "Δ A difference between a value and an expected value."
                    return ()
                return ()

            elAttr
                "div"
                ("id" =: "task-working")
                (text "")

    return ()

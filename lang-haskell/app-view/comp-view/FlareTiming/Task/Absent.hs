module FlareTiming.Task.Absent (tableAbsent) where

import Data.Maybe (isJust)
import Prelude hiding (abs)
import Reflex.Dom

import WireTypes.Route (TaskLength(..))
import WireTypes.Pilot (Pilot(..), Nyp(..), Dnf(..), DfNoTrack(..), Penal(..))
import WireTypes.Point (Breakdown(..), EssNotGoal(..))
import WireTypes.Penalty (PenaltySeqs(..))
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms (getTaskPilotAbs)
import FlareTiming.Pilot
    (rowPilot, rowDfNt, rowPenalJump, rowPenalEssNotGoal, rowPenalAuto, rowPenal)
import WireTypes.Comp (UtcOffset(..), EarlyStart(..))

tableAbsent
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t EarlyStart
    -> IxTask
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t Nyp
    -> Dynamic t Dnf
    -> Dynamic t DfNoTrack
    -> Dynamic t Penal
    -> Dynamic t Penal
    -> Dynamic t [(Pilot, Breakdown)]
    -> m ()
tableAbsent utc early ix ln nyp' dnf' dfNt' penalAuto' penal' sDf = do
    pb <- getPostBuild
    let nyp = unNyp <$> nyp'
    let dnf = unDnf <$> dnf'
    let dfNt = unDfNoTrack <$> dfNt'
    let penalAuto = unPenal <$> penalAuto'
    let penal = unPenal <$> penal'
    abs <- holdDyn [] =<< getTaskPilotAbs ix pb

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-vertical is-4" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    dyn_ $ ffor abs (\abs'' ->
                        if null abs''
                            then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "ABS"
                                    elClass "p" "subtitle" $ text "absent"
                                    el "p" $ text "There are no ABS pilots"
                            else
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "ABS"
                                    elClass "p" "subtitle" $ text "absent"
                                    elClass "div" "content" $ do
                                        _ <- elClass "table" "table is-striped is-narrow" $ do
                                                el "thead" $ do
                                                    el "tr" $ do
                                                        elClass "th" "th-pid" $ text "Id"
                                                        el "th" $ text "Name"

                                                el "tbody" $ simpleList abs rowPilot

                                        el "p" . text
                                            $ "A pilot's absence does not reduce the task validity."
                        )

                    dyn_ $ ffor dnf (\dnf'' ->
                        if null dnf''
                            then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "DNF"
                                    elClass "p" "subtitle" $ text "did not fly"
                                    el "p" $ text "There are no DNF pilots"
                            else
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "DNF"
                                    elClass "p" "subtitle" $ text "did not fly"
                                    elClass "div" "content" $ do
                                        _ <- elClass "table" "table is-striped is-narrow" $ do
                                                el "thead" $ do
                                                    el "tr" $ do
                                                        elClass "th" "th-pid" $ text "Id"
                                                        el "th" $ text "Name"

                                                el "tbody" $ simpleList dnf rowPilot

                                        el "p" . text
                                            $ "Both launch validity and task validity are reduced when pilots elect not to fly. The reduction is made by taking the fraction of those not flying over those present at launch."
                        )

                    dyn_ $ ffor nyp (\nyp'' ->
                        if null nyp''
                            then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "NYP"
                                    elClass "p" "subtitle" $ text "not yet processed"
                                    el "p" $ text "There are no NYP pilots"
                            else
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "NYP"
                                    elClass "p" "subtitle" $ text "not yet processed"
                                    elClass "div" "content" $ do
                                        _ <- elClass "table" "table is-striped is-narrow" $ do
                                                el "thead" $ do
                                                    el "tr" $ do
                                                        elClass "th" "th-pid" $ text "Id"
                                                        el "th" $ text "Name"

                                                el "tbody" $ simpleList nyp rowPilot

                                        el "p" . text
                                            $ "Unlike DNF pilots, these pilots do not decrease launch validity. When a task is not at full distance validity, if any one of the NYP pilots flew further then the task validity will increase when they are processed. Likewise for time validity and the fastest pilots being NYP."
                        )

                    dyn_ $ ffor dfNt (\dfNt'' ->
                        if null dfNt''
                            then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "DF"
                                    elClass "p" "subtitle" $ text "no track"
                                    el "p" $ text "There are no DF-no-track pilots"
                            else
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "DF"
                                    elClass "p" "subtitle" $ text "no track"
                                    elClass "div" "content" $ do
                                        _ <- elClass "table" "table is-striped is-narrow" $ do
                                                el "thead" $ do
                                                    el "tr" $ do
                                                        elClass "th" "th-pid" $ text "Id"
                                                        el "th" $ text "Name"
                                                        elClass "th" "th-awarded-start" $ text "Start"
                                                        elClass "th" "th-awarded-end" $ text "End"
                                                        elClass "th" "th-awarded-reach" $ text "Reach"

                                                el "tbody" $ simpleList dfNt (rowDfNt utc ln)

                                        el "p" . text
                                            $ "These pilots get awarded at least minimum distance."
                        )


        elClass "div" "tile is-vertical is-8" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    let jumpers = ffor sDf (\sDf' ->
                                (\(p, Breakdown{jump, penaltiesJumpRaw, penaltiesJumpEffective}) ->
                                    (p, penaltiesJumpRaw, penaltiesJumpEffective, jump))
                                <$> filter
                                        (\(_, Breakdown{jump, penaltiesJumpEffective = PenaltySeqs{adds}}) ->
                                            isJust jump || not (null adds))
                                        sDf')

                    let egs = ffor sDf (\sDf' ->
                                (\(p, Breakdown{penaltiesEssNotGoal}) ->
                                    (p, penaltiesEssNotGoal))
                                <$> filter
                                        (\(_, Breakdown{essNotGoal}) ->
                                            case essNotGoal of
                                                Nothing -> False
                                                Just (EssNotGoal b) -> b)
                                        sDf')

                    dyn_ $ ffor jumpers (\jumpers'' ->
                        if null jumpers'' then
                            elClass "article" "tile is-child notification is-warning" $ do
                                elClass "p" "title" $ text "Penal - Jump the Gun"
                                el "p" $ text "There are no \"jump the gun\" penalties"
                        else
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Penal - Jump the Gun"
                                elClass "p" "subtitle" $ text "what's a few points for a jump start?"
                                elClass "div" "content" $ do
                                    _ <- elClass "table" "table is-striped is-narrow" $ do
                                            el "thead" $ do
                                                el "tr" $ do
                                                    elAttr "th" ("colspan" =: "4") $ text ""
                                                    elAttr "th" ("class" =: "th-penalty-point-group" <> "colspan" =: "3") $ text "Points (so that ≮ minimum distance)"

                                                el "tr" $ do
                                                    elClass "th" "th-pid" $ text "Id"
                                                    el "th" $ text "Name"
                                                    elClass "th" "th-start-early" $ text "Early"
                                                    elClass "th" "th-penalty-reset" $ text "Reset"
                                                    elClass "th" "th-penalty-point" $ text "Toll"
                                                    elClass "th" "th-penalty-point" $ text "Rebate"
                                                    elClass "th" "th-penalty-point" $ text "Fee"

                                            el "tbody" $
                                                simpleList jumpers (rowPenalJump 3 (earliest <$> early))

                                    return ()

                                el "p" . text
                                    $ "These pilots were penalized (-) or rewarded (+).")

                    dyn_ $ ffor egs (\xs ->
                        if null xs then
                            elClass "article" "tile is-child notification is-warning" $ do
                                elClass "p" "title" $ text "Penal - ESS not Goal"
                                el "p" $ text "There are no \"ESS not goal\" penalties"
                        else
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Penal - ESS not Goal"
                                elClass "p" "subtitle" $ text "miss goal and miss out"
                                elClass "div" "content" $ do
                                    _ <- elClass "table" "table is-striped is-narrow" $ do
                                            el "thead" $ do
                                                el "tr" $ do
                                                    elClass "th" "th-pid" $ text "Id"
                                                    el "th" $ text "Name"
                                                    elClass "th" "th-penalty" $ text "Points"

                                            el "tbody" $ simpleList egs (rowPenalEssNotGoal 3)
                                    return ()

                                el "p" . text
                                    $ "These pilots were penalized (-) or rewarded (+).")

                    dyn_ $ ffor penalAuto (\xs ->
                        if null xs then
                            elClass "article" "tile is-child notification is-warning" $ do
                                elClass "p" "title" $ text "Penal - Auto"
                                el "p" $ text "There are no auto penalties"
                        else
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Penal - Auto"
                                elClass "p" "subtitle" $ text "getting jumpy"
                                elClass "div" "content" $ do
                                    _ <- elClass "table" "table is-striped is-narrow" $ do
                                            el "thead" $ do
                                                el "tr" $ do
                                                    elClass "th" "th-pid" $ text "Id"
                                                    el "th" $ text "Name"
                                                    elClass "th" "th-norm th-penalty" $ text "✓ Points"
                                                    elClass "th" "th-norm th-penalty-reason" $ text "Reason"

                                            el "tbody" $ simpleList penalAuto (rowPenalAuto 3)
                                    return ()

                                el "p" . text
                                    $ "These pilots were penalized (-) or rewarded (+).")

                    dyn_ $ ffor penal (\xs ->
                        if null xs then
                            elClass "article" "tile is-child notification is-warning" $ do
                                elClass "p" "title" $ text "Penal - Manual"
                                el "p" $ text "There are no penalties given out by the scorer"
                        else
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Penal - Manual"
                                elClass "p" "subtitle" $ text "at the scorer's discretion"
                                elClass "div" "content" $ do
                                    _ <- elClass "table" "table is-striped is-narrow" $ do
                                            el "thead" $ do
                                                el "tr" $ do
                                                    elClass "th" "th-pid" $ text "Id"
                                                    el "th" $ text "Name"
                                                    elClass "th" "th-penalty" $ text "Fraction"
                                                    elClass "th" "th-penalty" $ text "Points"
                                                    elClass "th" "th-penalty-reason" $ text "Reason"

                                            el "tbody" $ simpleList penal (rowPenal 3)
                                    return ()

                                el "p" . text
                                    $ "These pilots were penalized (-) or rewarded (+).")

    return ()

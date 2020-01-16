module FlareTiming.Plot.Weight.Working (viewWeightWorking) where

import Reflex
import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy (Validity(..), TaskValidity(..), showTaskValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , PilotsFlying(..)
    )
import WireTypes.Route (TaskLength(..), TaskDistance(..), showTaskDistance)
import WireTypes.Comp (Discipline(..), Tweak(..), LwScaling(..), AwScaling(..), scaling)
import WireTypes.Point
    ( GoalRatio(..)
    , Allocation(..)
    , Weights(..)
    , DistanceWeight(..)
    , ArrivalWeight(..)
    , LeadingWeight(..)
    , TimeWeight(..)
    , Points(..)
    , DistancePoints(..)
    , ArrivalPoints(..)
    , LeadingPoints(..)
    , TimePoints(..)
    , PilotDistance(..)
    , ReachToggle(..)
    )
import FlareTiming.Katex (Expect(..), Recalc(..), katexNewLine, katexCheck)

textf :: String -> Double -> T.Text
textf fmt d = T.pack $ printf fmt d

hookWorking
    :: Discipline
    -> Maybe Tweak
    -> Vy.TaskValidity
    -> PilotsFlying
    -> GoalRatio
    -> Maybe LwScaling
    -> Maybe AwScaling
    -> TaskDistance
    -> PilotDistance
    -> Weights
    -> Points
    -> T.Text
hookWorking hgOrPg tweak tv pf gr lws aws td bd w p =
    weightWorking hgOrPg tweak pf gr lws aws td bd w <> pointWorking tv w p

weightWorking
    :: Discipline
    -> Maybe Tweak
    -> PilotsFlying
    -> GoalRatio
    -> Maybe LwScaling
    -> Maybe AwScaling
    -> TaskDistance
    -> PilotDistance
    -> Weights
    -> T.Text
weightWorking
    hgOrPg
    tweak
    (PilotsFlying pf)
    (GoalRatio gr)
    lws
    aws
    (TaskDistance td)
    (PilotDistance bd)
    Weights
        { distance = DistanceWeight dw
        , arrival = ArrivalWeight aw
        , leading = LeadingWeight lw
        , time = TimeWeight tw
        } =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> katexNewLine
    <> " gr &= \\\\frac{pg}{pf}"
    <> katexNewLine
    <> (" &= \\\\frac{" <> (textf "%.0f" pg) <> "}{" <> (T.pack . show $ pf) <> "}")
    <> katexNewLine
    <> katexGoalRatio gr
    <> katexCheck 3 (Recalc gr') (Expect gr)
    <> katexNewLine
    <> katexNewLine

    <> " dw &= 0.9 - 1.665 * gr + 1.713 * gr^2 - 0.587 * gr^3"
    <> katexNewLine
    <> katexDistanceWeight gr
    <> katexCheck 3 (Recalc dw') (Expect dw)
    <> katexNewLine
    <> katexNewLine

    <> katexLeadingWeight hgOrPg gr
    <> katexCheck 3 (Recalc lw') (Expect lw)
    <> katexNewLine
    <> katexNewLine

    <> arrivalWeightCases
    <> katexArrivalWeight'
    <> katexCheck 3 (Recalc aw') (Expect aw)
    <> katexNewLine
    <> katexNewLine

    <> katexTimeWeight lw aw'
    <> katexCheck 3 (Recalc tw') (Expect tw)
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-weight-working')"
    <> ", {throwOnError: false});"
    where
        pg = gr * fromIntegral pf
        gr' = pg / fromIntegral pf

        gr2 = gr * gr
        gr3 = gr * gr2
        dw1 = 1.665 * gr
        dw2 = 1.713 * gr2
        dw3 = 0.587 * gr3
        dw' = 0.9 - dw1 + dw2 - dw3

        Tweak{arrivalRank, arrivalTime} = scaling HangGliding tweak
        awScale =
            if | arrivalRank -> 8
               | arrivalTime -> 4
               | otherwise -> 0
        awScaleText = T.pack $ show awScale
        katexArrivalWeight' =
            if arrivalRank || arrivalTime
                then katexArrivalWeight hgOrPg
                else katexArrivalWeightZero

        lw' =
            if lw == 0 then 0 else
            case (hgOrPg, gr) of
                (Paragliding, 0) -> bd / td * 0.1
                (Paragliding, _) -> ((1 - dw') / 8) * 1.4 * (maybe 1 (\(LwScaling x) -> x) $ lws)
                (HangGliding, _) -> ((1 - dw') / 8) * 1.4

        aw' =
            if | aw == 0 -> 0
               | not (arrivalRank || arrivalTime) -> 0
               | otherwise -> (1 - dw') / awScale

        tw' = 1 - dw' - lw' - aw'

        katexGoalRatio 0 =
            " &= 0"
        katexGoalRatio _gr =
            " &= " <> textf "%.3f" gr'

        katexDistanceWeight 0 =
            " &= 0.9"
        katexDistanceWeight _gr =
            (" &= 0.9 - 1.665 * " <> (T.pack $ printf "%.3f + 1.713 * %.3f - 0.587 * %.3f" gr gr2 gr3))
            <> katexNewLine
            <> (" &= 0.9 - " <> (T.pack $ printf "%.3f + %.3f - %.3f" dw1 dw2 dw3))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" dw')

        leadingWeightCases HangGliding =
            " lw &= \\\\frac{1 - dw}{8} * 1.4 * lws"
        leadingWeightCases Paragliding =
            " lw &="
            <> " \\\\begin{cases}"
            <> " \\\\dfrac{bd}{td} * 0.1"
            <> " &\\\\text{if gr = 0}"
            <> katexNewLine
            <> katexNewLine
            <> " \\\\dfrac{1 - dw}{8} * 1.4 * lws"
            <> " &\\\\text{otherwise}"
            <> " \\\\end{cases}"
            <> katexNewLine

        arrivalWeightCases =
            " aw &="
            <> " \\\\begin{cases}"
            <> " \\\\dfrac{1 - dw}{8} * aws"
            <> " &\\\\text{if hang gliding with arrival position points}"
            <> katexNewLine
            <> " \\\\dfrac{1 - dw}{4} * aws"
            <> " &\\\\text{if hang gliding with arrival time points}"
            <> katexNewLine
            <> " 0"
            <> " &\\\\text{if hang gliding without arrival points}"
            <> katexNewLine
            <> " 0"
            <> " &\\\\text{if paragliding}"
            <> " \\\\end{cases}"
            <> katexNewLine

        katexLeadingWeight dp@HangGliding _gr =
            leadingWeightCases dp
            <> katexNewLine
            <> (" &= \\\\frac{1 - " <> textf "%.3f" dw' <> "}{8} * 1.4" <> maybe "" (\(LwScaling x) -> if x == 0 then "* 0" else textf "* %.3f" x) lws)
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')
        katexLeadingWeight dp@Paragliding 0 =
            leadingWeightCases dp
            <> " &= \\\\frac{bd}{td} * 0.1"
            <> katexNewLine
            <> (" &= \\\\frac{" <> textf "%.3f" bd <> "}{" <> textf "%.3f" td <> "} * 0.1")
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')
        katexLeadingWeight dp@Paragliding _gr =
            leadingWeightCases dp
            <> " &= \\\\frac{1 - dw}{8} * 1.4 * lws"
            <> katexNewLine
            -- TODO: Use scaling factor for leading weight.
            <> (" &= \\\\frac{1 - " <> textf "%.3f" dw' <> "}{8} * 1.4" <> maybe "" (\(LwScaling x) -> if x == 0 then "* 0" else textf "* %.3f" x) lws)
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')

        katexArrivalWeightZero =
            " &= 0"

        katexArrivalWeight Paragliding =
            " &= 0"
        katexArrivalWeight HangGliding =
            " &= \\\\frac{1 - dw}{" <> awScaleText <> "} * aws"
            <> katexNewLine
            <> (" &= \\\\frac{1 - " <> textf "%.3f" dw' <> "}{" <> awScaleText <> "}" <> maybe "" (\(AwScaling x) -> if x == 0 then "* 0" else textf "* %.3f" x) aws)
            <> katexNewLine
            <> (" &= " <> textf "%.3f" aw')

        katexTimeWeight 0 0 =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - 0 - 0" dw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw')
        katexTimeWeight _lw 0 =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - %.3f - 0" dw' lw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw')
        katexTimeWeight 0 _aw =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - 0 - %.3f" dw' aw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw')
        katexTimeWeight _lw _aw =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - %.3f - %.3f" dw' lw' aw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw')

pointWorking :: Vy.TaskValidity -> Weights -> Points -> T.Text
pointWorking
    tv@(Vy.TaskValidity tv')
    Weights
        { distance = DistanceWeight dw
        , arrival = ArrivalWeight aw
        , leading = LeadingWeight lw
        , time = TimeWeight tw
        }
    Points
        { distance = DistancePoints dp
        , arrival = ArrivalPoints ap
        , leading = LeadingPoints lp
        , time = TimePoints tp
        } =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> katexNewLine
    <> " k &= 1000 * tv"
    <> katexNewLine
    <> (" &= 1000 * " <> Vy.showTaskValidity tv)
    <> katexNewLine
    <> (" &= " <> k)
    <> katexNewLine
    <> katexNewLine

    <> " dp &= k * dw"
    <> katexNewLine
    <> (" &= " <> k <> textf " * %.3f" dw)
    <> katexNewLine
    <> (" &= " <> textf "%.2f" dp)
    <> katexNewLine
    <> katexNewLine

    <> " lp &= k * lw"
    <> katexNewLine
    <> (" &= " <> k <> textf " * %.3f" lw)
    <> katexNewLine
    <> (" &= " <> textf "%.2f" lp)
    <> katexNewLine
    <> katexNewLine

    <> " ap &= k * aw"
    <> katexNewLine
    <> (" &= " <> k <> textf " * %.3f" aw)
    <> katexNewLine
    <> (" &= " <> textf "%.2f" ap)
    <> katexNewLine
    <> katexNewLine

    <> " tp &= k * tw"
    <> katexNewLine
    <> (" &= " <> k <> textf " * %.3f" tw)
    <> katexNewLine
    <> (" &= " <> textf "%.2f" tp)
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-point-working')"
    <> ", {throwOnError: false});"
    where
        k = textf "%.0f" $ 1000.0 * tv'

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "spacer" $ return ()

viewWeightWorking
    :: MonadWidget t m
    => Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t (Maybe TaskLength)
    -> m ()
viewWeightWorking hgOrPg twk' vy' vw' al' ln' = do
    _ <- dyn $ ffor3 twk' vy' vw' (\twk vy vw -> do
        _ <- dyn $ ffor2 al' ln' (\al ln ->
            case (vy, vw, twk, al, ln) of
                (Nothing, _, _, _, _) -> text "Loading validity ..."
                (_, Nothing, _, _, _) -> text "Loading validity working ..."
                (_, _, Nothing, _, _) -> text "Loading competition tweaks ..."
                (_, _, _, Nothing, _) -> text "Loading allocations ..."
                (_, _, _, _, Nothing) -> text "Loading task length ..."
                ( Just Vy.Validity{task}
                    , Just
                        ValidityWorking
                            { launch =
                                LaunchValidityWorking { flying = pf@(PilotsFlying pf')}
                            , distance =
                                DistanceValidityWorking
                                    { reachMax = ReachToggle{extra = bd}
                                    }
                            }
                    , Just
                        Tweak
                            { leadingWeightScaling = lwScaling
                            , arrivalRank
                            , arrivalTime
                            }
                    , Just alloc
                    , Just TaskLength{taskRoute = td}
                    ) -> do

                    let gr@(GoalRatio gr') = goalRatio alloc

                    let wts@Weights
                            { distance = DistanceWeight dw
                            , arrival = ArrivalWeight aw
                            , leading = LeadingWeight lw
                            , time = TimeWeight tw
                            } = weight alloc

                    let pts@Points
                            { distance = DistancePoints dp
                            , arrival = ArrivalPoints ap
                            , leading = LeadingPoints lp
                            , time = TimePoints tp
                            } = points alloc

                    let awScaling =
                            Just . AwScaling $
                            if | hgOrPg == Paragliding -> 0
                               | arrivalRank -> 1.0
                               | arrivalTime -> 1.0
                               | otherwise -> 0.0

                    let pg = gr' * fromIntegral pf'
                    let lwS = show lwScaling
                    let awS = show awScaling

                    elAttr
                        "a"
                        (  ("class" =: "button")
                        <> ("onclick" =: hookWorking hgOrPg twk task pf gr lwScaling awScaling td bd wts pts)
                        )
                        (text "Show Working")

                    spacer

                    elClass "div" "card" $ do
                        elClass "div" "card-content" $ do
                            elClass "p" "subtitle is-6" $ do
                                el "span" $
                                    elClass "h2" "title is-4" $ text "Weights"

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pf = pilots flying"
                                        elClass "span" "tag is-dark" . text
                                            $ (T.pack . show $ pf)
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pg = pilots in goal"
                                        elClass "span" "tag is-warning" . text
                                            $ textf "%.0f" pg
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "gr = goal ratio"
                                        elClass "span" "tag is-primary" . text
                                            $ textf "%.3f" gr'

                            if hgOrPg == Paragliding then do
                                elClass "div" "field is-grouped is-grouped-multiline" $ do
                                    elClass "div" "control" $ do
                                        elClass "div" "tags has-addons" $ do
                                            elClass "span" "tag" $ do text "td = task distance"
                                            elClass "span" "tag is-info"
                                                $ text (showTaskDistance td)
                                elClass "div" "field is-grouped is-grouped-multiline" $ do
                                    elClass "div" "control" $ do
                                        elClass "div" "tags has-addons" $ do
                                            elClass "span" "tag" $ do text "bd = best distance"
                                            elClass "span" "tag is-dark"
                                                $ text (T.pack . show $ bd)
                            else
                                return ()

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lws = leading weight scaling"
                                        elClass "span" "tag is-danger" . text
                                            $ T.pack lwS
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "aws = arrival weight scaling"
                                        elClass "span" "tag is-warning" . text
                                            $ T.pack awS

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "dw = distance weight"
                                        elClass "span" "tag is-info" . text
                                            $ textf "%.3f" dw
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tw = time weight"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.3f" tw
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lw = leading weight"
                                        elClass "span" "tag is-danger" . text
                                            $ textf "%.3f" lw
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "aw = arrival weight"
                                        elClass "span" "tag is-warning" . text
                                            $ textf "%.3f" aw

                            elAttr
                                "div"
                                ("id" =: "alloc-weight-working")
                                (text "")

                    spacer

                    elClass "div" "card" $ do
                        elClass "div" "card-content" $ do
                            elClass "p" "subtitle is-6" $ do
                                el "span" $
                                    elClass "h2" "title is-4" $ text "Points"

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tv = task validity"
                                        elClass "span" "tag is-black" . text
                                            $ Vy.showTaskValidity task

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "dp = distance points"
                                        elClass "span" "tag is-info" . text
                                            $ textf "%.2f" dp
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tp = time points"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.2f" tp
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lp = leading points"
                                        elClass "span" "tag is-danger" . text
                                            $ textf "%.2f" lp
                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "ap = arrival points"
                                        elClass "span" "tag is-warning" . text
                                            $ textf "%.2f" ap

                            elAttr
                                "div"
                                ("id" =: "alloc-point-working")
                                (text "")

                    return ())
        return ())
    return ()

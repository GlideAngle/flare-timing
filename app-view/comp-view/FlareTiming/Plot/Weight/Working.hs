module FlareTiming.Plot.Weight.Working (viewWeightWorking) where

import Text.Printf (printf)
import Reflex
import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy (Validity(..), TaskValidity(..), showTaskValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , PilotsFlying(..)
    , MaximumDistance(..)
    )
import WireTypes.Route (TaskLength(..), TaskDistance(..), showTaskDistance)
import WireTypes.Comp (Discipline(..), Tweak(..))
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
    , zeroWeights
    )

textf :: String -> Double -> T.Text
textf fmt d = T.pack $ printf fmt d

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

hookWorking
    :: Discipline
    -> Vy.TaskValidity
    -> PilotsFlying
    -> GoalRatio
    -> TaskDistance
    -> MaximumDistance
    -> Weights
    -> Points
    -> T.Text
hookWorking hgOrPg tv pf gr td bd w p =
    weightWorking hgOrPg pf gr td bd w <> pointWorking tv w p

weightWorking
    :: Discipline
    -> PilotsFlying
    -> GoalRatio
    -> TaskDistance
    -> MaximumDistance
    -> Weights
    -> T.Text
weightWorking
    hgOrPg
    (PilotsFlying pf)
    (GoalRatio gr)
    (TaskDistance td)
    (MaximumDistance bd)
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
    <> katexNewLine
    <> katexNewLine

    <> " dw &= 0.9 - 1.665 * gr + 1.713 * gr^2 - 0.587 * gr^3"
    <> katexNewLine
    <> katexDistanceWeight gr
    <> katexNewLine
    <> katexNewLine

    <> katexLeadingWeight hgOrPg gr lw
    <> katexNewLine
    <> katexNewLine

    <> katexArrivalWeight aw
    <> katexNewLine
    <> katexNewLine

    <> katexTimeWeight lw aw
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-weight-working')"
    <> ", {throwOnError: false});"
    where
        gr2 = gr * gr
        gr3 = gr * gr2
        dw1 = 1.665 * gr
        dw2 = 1.713 * gr2
        dw3 = 0.587 * gr3
        pg = gr * fromIntegral pf

        katexGoalRatio 0 =
            " &= 0"
        katexGoalRatio gr' =
            " &= " <> textf "%.3f" gr'

        katexDistanceWeight 0 =
            " &= 0.9"
        katexDistanceWeight _gr =
            (" &= 0.9 - 1.665 * " <> (T.pack $ printf "%.3f + 1.713 * %.3f - 0.587 * %.3f" gr gr2 gr3))
            <> katexNewLine
            <> (" &= 0.9 - " <> (T.pack $ printf "%.3f + %.3f - %.3f" dw1 dw2 dw3))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" dw)

        leadingWeightCases =
            " lw &="
            <> " \\\\begin{cases}"
            <> " \\\\dfrac{bd}{td} * 0.1"
            <> " &\\\\text{if gr = 0}"
            <> katexNewLine
            <> katexNewLine
            <> " \\\\dfrac{1 - dw}{8} * 1.4 * 2"
            <> " &\\\\text{otherwise}"
            <> " \\\\end{cases}"
            <> katexNewLine

        katexLeadingWeight _ _ 0 =
            " lw &= 0"
        katexLeadingWeight HangGliding _gr lw' =
            leadingWeightCases
            <> " &= \\\\frac{1 - dw}{8} * 1.4"
            <> katexNewLine
            <> (" &= \\\\frac{1 - " <> textf "%.3f" dw <> "}{8} * 1.4")
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')
        katexLeadingWeight Paragliding 0 lw' =
            leadingWeightCases
            <> " &= \\\\frac{bd}{td} * 0.1"
            <> katexNewLine
            <> (" &= \\\\frac{" <> textf "%.3f" bd <> "}{" <> textf "%.3f" td <> "} * 0.1")
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')
        katexLeadingWeight Paragliding _gr lw' =
            leadingWeightCases
            <> " &= \\\\frac{1 - dw}{8} * 1.4 * 2"
            <> katexNewLine
            -- TODO: Use scaling factor for leading weight.
            <> (" &= \\\\frac{1 - " <> textf "%.3f" dw <> "}{8} * 1.4 * 2")
            <> katexNewLine
            <> (" &= " <> textf "%.3f" lw')

        katexArrivalWeight 0 =
            " aw &= 0"
        katexArrivalWeight aw' =
            " aw &= \\\\frac{1 - dw}{8}"
            <> katexNewLine
            <> " &= \\\\frac{1 - " <> textf "%.3f" dw <> "}{8}"
            <> katexNewLine
            <> (" &= " <> textf "%.3f" aw')

        katexTimeWeight 0 0 =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - 0 - 0" dw))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw)
        katexTimeWeight lw' 0 =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - %.3f - 0" dw lw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw)
        katexTimeWeight 0 aw' =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - 0 - %.3f" dw aw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw)
        katexTimeWeight lw' aw' =
            " tw &= 1 - dw - lw - aw"
            <> katexNewLine
            <> (" &= 1 - " <> (T.pack $ printf "%.3f - %.3f - %.3f" dw lw' aw'))
            <> katexNewLine
            <> (" &= " <> textf "%.3f" tw)

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
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t (Maybe TaskLength)
    -> m ()
viewWeightWorking hgOrPg vy' vw' tw' al' ln' = do
    _ <- dyn $ ffor3 vy' vw' tw' (\vy vw tw -> do
        _ <- dyn $ ffor2 al' ln' (\al ln ->
            case (vy, vw, tw, al, ln) of
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
                                DistanceValidityWorking{bestDistance = bd}
                            }
                    , Just tweak
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

                    let pg = gr' * fromIntegral pf'

                    elAttr
                        "a"
                        (("class" =: "button") <> ("onclick" =: hookWorking hgOrPg task pf gr td bd wts pts))
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
                                        elClass "span" "tag" $ do text "gr = goal ratio"
                                        elClass "span" "tag is-primary" . text
                                            $ textf "%.3f" gr'
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pf = pilots flying"
                                        elClass "span" "tag is-danger" . text
                                            $ (T.pack . show $ pf)
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pg = pilots in goal"
                                        elClass "span" "tag is-warning" . text
                                            $ textf "%.0f" pg

                            if hgOrPg == Paragliding
                                then
                                    elClass "div" "field is-grouped is-grouped-multiline" $ do
                                        elClass "div" "control" $ do
                                            elClass "div" "tags has-addons" $ do
                                                elClass "span" "tag" $ do text "td = task distance"
                                                elClass "span" "tag is-info"
                                                    $ text (showTaskDistance td)
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
                                        elClass "span" "tag" $ do text "dw = distance weight"
                                        elClass "span" "tag is-info" . text
                                            $ textf "%.3f" dw
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tw = time weight"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.3f" tw
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lw = leading weight"
                                        elClass "span" "tag is-danger" . text
                                            $ textf "%.3f" lw
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
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tp = time points"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.2f" tp
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lp = leading points"
                                        elClass "span" "tag is-danger" . text
                                            $ textf "%.2f" lp
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

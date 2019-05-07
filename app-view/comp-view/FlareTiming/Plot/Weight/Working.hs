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
    )
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

hookWorking :: Vy.TaskValidity -> Weights -> Points -> T.Text
hookWorking tv w p =
    weightWorking w p <> pointWorking tv w p

weightWorking :: Weights -> Points -> T.Text
weightWorking _ _ =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> katexNewLine
    <> " gr &= \\\\frac{pg}{pf}"
    <> katexNewLine
    <> katexNewLine
    <> " dw &= 0.9 - 1.665 * gr + 1.713 * gr^2 - 0.587 * gr^3"
    <> katexNewLine
    <> " lw &= \\\\frac{1 - dw}{8} * 1.4"
    <> katexNewLine
    <> " aw &= \\\\frac{1 - dw}{8}"
    <> katexNewLine
    <> " tw &= 1 - dw - lw - aw"
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-weight-working')"
    <> ", {throwOnError: false});"

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
    -> m ()
viewWeightWorking hgOrPg vy' vw' tw' al' = do
    _ <- dyn $ ffor3 vy' tw' al' (\vy tw al -> do
        _ <- dyn $ ffor vw' (\vw ->
            case (vy, vw, tw, al) of
                (Nothing, _, _, _) -> text "Loading validity ..."
                (_, Nothing, _, _) -> text "Loading validity working ..."
                (_, _, Nothing, _) -> text "Loading competition tweaks ..."
                (_, _, _, Nothing) -> text "Loading allocations ..."
                (Just Vy.Validity{task}, Just ValidityWorking{launch = LaunchValidityWorking{flying}}, Just tweak, Just alloc) -> do
                    let (GoalRatio gr) = goalRatio alloc

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

                    elAttr
                        "a"
                        (("class" =: "button") <> ("onclick" =: hookWorking task wts pts))
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
                                            $ textf "%.3f" gr
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pf = pilots flying"
                                        elClass "span" "tag is-danger" . text
                                            $ (T.pack . show $ flying)
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pg = pilots in goal"
                                        elClass "span" "tag is-warning" . text
                                            $ ""

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

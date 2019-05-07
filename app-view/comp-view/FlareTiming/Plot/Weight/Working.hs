module FlareTiming.Plot.Weight.Working (viewWeightWorking) where

import Text.Printf (printf)
import Reflex
import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy (Validity(..), showTaskValidity)
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

hookWorking :: Tweak -> Allocation -> T.Text
hookWorking tw al =
    weightWorking tw al <> pointWorking tw al

weightWorking :: Tweak -> Allocation -> T.Text
weightWorking _ _ =
    "katex.render("
    <> "\"\\\\begin{aligned} "
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

pointWorking :: Tweak -> Allocation -> T.Text
pointWorking _ _ =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " k &= 1000 * tv"
    <> katexNewLine
    <> " dp &= k * dw"
    <> katexNewLine
    <> " lp &= k * lw"
    <> katexNewLine
    <> " ap &= k * aw"
    <> katexNewLine
    <> " tp &= k * tw"
    <> katexNewLine
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-point-working')"
    <> ", {throwOnError: false});"

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

                    let Weights
                            { distance = DistanceWeight dw
                            , arrival = ArrivalWeight aw
                            , leading = LeadingWeight lw
                            , time = TimeWeight tw
                            } = weight alloc

                    let Points
                            { distance = DistancePoints dp
                            , arrival = ArrivalPoints ap
                            , leading = LeadingPoints lp
                            , time = TimePoints tp
                            } = points alloc

                    elAttr
                        "a"
                        (("class" =: "button") <> ("onclick" =: hookWorking tweak alloc))
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
                                            $ textf "%.3f" dp
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tp = time points"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.3f" tp
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lp = leading points"
                                        elClass "span" "tag is-danger" . text
                                            $ textf "%.3f" lp
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "ap = arrival points"
                                        elClass "span" "tag is-warning" . text
                                            $ textf "%.3f" ap

                            elAttr
                                "div"
                                ("id" =: "alloc-point-working")
                                (text "")

                    return ())
        return ())
    return ()

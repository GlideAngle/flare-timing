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
    , zeroWeights
    )

textf :: String -> Double -> T.Text
textf fmt d = T.pack $ printf fmt d

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

hookWorking :: Tweak -> Allocation -> T.Text
hookWorking tweak alloc =
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
    <> katexNewLine
    <> katexNewLine
    <> " p &= 1000 * tv"
    <> katexNewLine
    <> katexNewLine
    <> " dp &= p * dw"
    <> katexNewLine
    <> " lp &= p * lw"
    <> katexNewLine
    <> " ap &= p * aw"
    <> katexNewLine
    <> " tp &= p * tw"
    <> katexNewLine
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-working')"
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
                                        elClass "span" "tag" $ do text "tv = task validity"
                                        elClass "span" "tag is-info" . text
                                            $ Vy.showTaskValidity task
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "gr = goal ratio"
                                        elClass "span" "tag is-success" . text
                                            $ textf "%.3f" gr
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pg = pilots in goal"
                                        elClass "span" "tag is-primary" . text
                                            $ ""
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "pf = pilots flying"
                                        elClass "span" "tag is-primary" . text
                                            $ (T.pack . show $ flying)
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
                                        elClass "span" "tag is-black" . text
                                            $ textf "%.3f" aw

                    spacer

                    elClass "div" "card" $ do
                        elClass "div" "card-content" $ do
                            elClass "p" "subtitle is-6" $ do
                                el "span" $
                                    elClass "h2" "title is-4" $ text "Points"

                            elClass "div" "field is-grouped is-grouped-multiline" $ do
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "dp = distance points"
                                        elClass "span" "tag is-primary" . text
                                            $ ""
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "lp = leading points"
                                        elClass "span" "tag is-primary" . text
                                            $ ""
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "ap = arrival points"
                                        elClass "span" "tag is-primary" . text
                                            $ ""
                                elClass "div" "control" $ do
                                    elClass "div" "tags has-addons" $ do
                                        elClass "span" "tag" $ do text "tp = time points"
                                        elClass "span" "tag is-primary" . text
                                            $ ""

                                elAttr
                                    "div"
                                    ("id" =: "alloc-working")
                                    (text "")

                    return ())
        return ())
    return ()

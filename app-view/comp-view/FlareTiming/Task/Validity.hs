module FlareTiming.Task.Validity (viewValidity) where

import Prelude hiding (sum)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    (Validity(..), showLaunchValidity, showDistanceValidity, showTimeValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    )

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

hookWorking :: Vy.Validity -> ValidityWorking -> T.Text
hookWorking v ValidityWorking{distance = w} =
    launchWorking v <> distanceWorking v w <> timeWorking v

launchWorking :: Vy.Validity -> T.Text
launchWorking vy =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{f}{p * n})"
    <> katexNewLine
    <> "validity &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> katexNewLine
    <> " &= "
    <> (Vy.showLaunchValidity . Vy.launch $ vy)
    <> " \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
    <> ", {throwOnError: false});"

distanceWorking :: Vy.Validity -> DistanceValidityWorking -> T.Text
distanceWorking v w =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " sum"
    <> " &="
    <> " \\\\sum_p \\\\max(0, FlownDist_p - md)"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ sum w)
    <> katexNewLine
    <> " a &= (ng + 1) * (nd - md)"
    <> katexNewLine
    <> " b &="
    <> " \\\\max(0, ng * (bd- nd)"
    <> katexNewLine
    <> "area"
    <> " &="
    <> "\\\\frac{(a + b)}{2}"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ area w)
    <> katexNewLine
    <> " validity &="
    <> " \\\\min(1, \\\\frac{sum}{f * area})"
    <> katexNewLine
    <> " &= "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('distance-working')"
    <> ", {throwOnError: false});"

timeWorking :: Vy.Validity -> T.Text
timeWorking vy =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " x &="
    <> " \\\\begin{cases}"
    <> " \\\\dfrac{bt}{nt}"
    <> " &\\\\text{if one pilot reached ESS}"
    <> katexNewLine
    <> katexNewLine
    <> " \\\\dfrac{bd}{nd}"
    <> " &\\\\text{if no pilot reached ESS}"
    <> " \\\\end{cases}"
    <> katexNewLine
    <> " y &= \\\\min(1, x)"
    <> katexNewLine
    <> "validity &= \\\\max(0, \\\\min(1, -0.271 + 2.912 * y - 2.098 * y^2 + 0.457 * y^3))"
    <> katexNewLine
    <> " &= "
    <> (Vy.showTimeValidity . Vy.time $ vy)
    <> " \\\\end{aligned}\""
    <> ", getElementById('time-working')"
    <> ", {throwOnError: false});"

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "spacer" $ return ()

viewValidity
    :: MonadWidget t m
    => Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> m ()
viewValidity vy vw = do
    _ <- dyn $ ffor2 vy vw (\x y ->
        case (x, y) of
            (Nothing, _) -> text "Loading validity ..."
            (_, Nothing) -> text "Loading validity workings ..."
            (Just v, Just w) -> do
                elAttr
                    "a"
                    (("class" =: "button") <> ("onclick" =: hookWorking v w))
                    (text "Show Working")

                spacer
                viewLaunch w
                spacer
                viewDistance w
                spacer
                viewTime w
                spacer
                return ())

    return ()

viewLaunch
    :: DomBuilder t m
    => ValidityWorking
    -> m ()
viewLaunch ValidityWorking{launch = LaunchValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" $ text "Launch Validity"
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "f = pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "p = pilots present"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ present)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "n = nominal launch"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalLaunch)

            elAttr
                "div"
                ("id" =: "launch-working")
                (text "")

    return ()

viewDistance
    :: DomBuilder t m
    => ValidityWorking
    -> m ()
viewDistance ValidityWorking{distance = DistanceValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" $ text "Distance Validity"
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "f = pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "area"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ area)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "ng = nominal goal"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalGoal)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "sum = sum of distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ sum)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nd = nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "md = minimum distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ minimumDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bd = best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)

            elAttr
                "div"
                ("id" =: "distance-working")
                (text "")

    return ()

viewTime
    :: DomBuilder t m
    => ValidityWorking
    -> m ()
viewTime ValidityWorking{time = TimeValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" $ text "Time Validity"
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "ss best time"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ ssBestTime)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bt = gs best time"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ gsBestTime)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nt = nominal time"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalTime)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bd = best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nd = nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)

            elAttr
                "div"
                ("id" =: "time-working")
                (text "")

    return ()

module FlareTiming.Task.Validity (viewValidity) where

import Prelude hiding (sum)
import Reflex
import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    (Validity(..), showLaunchValidity, showDistanceValidity, showTimeValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    , BestTime(..)
    )

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

hookWorking :: Vy.Validity -> ValidityWorking -> T.Text
hookWorking v ValidityWorking{launch = l, distance = d, time = t} =
    launchWorking v l <> distanceWorking v d <> timeWorking v t

launchWorking :: Vy.Validity -> LaunchValidityWorking -> T.Text
launchWorking v w@LaunchValidityWorking{flying = f} =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{f}{p * n})"
    <> katexNewLine
    <> " &= \\\\min(1, \\\\frac{"
    <> (T.pack . show $ f)
    <> "}{"
    <> (T.pack . show $ present w)
    <> " * "
    <> (T.pack . show $ nominalLaunch w)
    <> "})"
    <> katexNewLine
    <> katexNewLine
    <> "validity &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> katexNewLine
    <> " &= "
    <> (Vy.showLaunchValidity . Vy.launch $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
    <> ", {throwOnError: false});"

distanceWorking :: Vy.Validity -> DistanceValidityWorking -> T.Text
distanceWorking v w =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " sum"
    <> " &="
    <> " \\\\sum_p \\\\max(0, d_p - md)"
    <> " &\\\\"
    <> " d_p = \\\\text{distance flown by pilot \\\\textit{p}}"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ sum w)
    <> katexNewLine
    <> katexNewLine
    <> " a &= (ng + 1) * (nd - md)"
    <> katexNewLine
    <> katexNewLine
    <> " b &="
    <> " \\\\max(0, ng * (bd- nd)"
    <> katexNewLine
    <> katexNewLine
    <> "area"
    <> " &="
    <> "\\\\frac{(a + b)}{2}"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ area w)
    <> katexNewLine
    <> katexNewLine
    <> " validity &="
    <> " \\\\min(1, \\\\frac{sum}{f * area})"
    <> katexNewLine
    <> " &= "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('distance-working')"
    <> ", {throwOnError: false});"

timeWorkingCase :: (Semigroup p, IsString p) => Maybe a -> p
timeWorkingCase (Just _) = " &= \\\\dfrac{bt}{nt}"
timeWorkingCase Nothing = " &= \\\\dfrac{bd}{nd}"

timeWorkingSub :: TimeValidityWorking -> T.Text

timeWorkingSub
    TimeValidityWorking{gsBestTime = Just bt@(BestTime b), nominalTime = nt} =
    " &="
    <> " \\\\dfrac{"
    <> (T.pack . show $ bt)
    <> "}{"
    <> (T.pack . show $ nt)
    <> "}"
    <> katexNewLine
    <> " &="
    <> " \\\\dfrac{"
    <> (T.pack . printf "%f h" $ b)
    <> "}{"
    <> (T.pack . show $ nt)
    <> "}"

timeWorkingSub
    TimeValidityWorking{bestDistance = bd, nominalDistance = nd} =
    " &="
    <> " \\\\dfrac{"
    <> (T.pack . show $ bd)
    <> "}{"
    <> (T.pack . show $ nd)
    <> "}"

timeWorking :: Vy.Validity -> TimeValidityWorking -> T.Text
timeWorking v w@TimeValidityWorking{gsBestTime = bt} =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " x &="
    <> " \\\\begin{cases}"
    <> " \\\\dfrac{bt}{nt}"
    <> " &\\\\text{if at least one pilot reached ESS}"
    <> katexNewLine
    <> katexNewLine
    <> " \\\\dfrac{bd}{nd}"
    <> " &\\\\text{if no pilots reached ESS}"
    <> " \\\\end{cases}"
    <> katexNewLine
    <> timeWorkingCase bt
    <> katexNewLine
    <> timeWorkingSub w
    <> katexNewLine
    <> katexNewLine
    <> " y &= \\\\min(1, x)"
    <> katexNewLine
    <> katexNewLine
    <> " z &= -0.271 + 2.912 * y - 2.098 * y^2 + 0.457 * y^3"
    <> katexNewLine
    <> katexNewLine
    <> "validity &= \\\\max(0, \\\\min(1, z))"
    <> katexNewLine
    <> " &= "
    <> (Vy.showTimeValidity . Vy.time $ v)
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
                viewLaunch v w
                spacer
                viewDistance v w
                spacer
                viewTime v w
                spacer
                return ())

    return ()

viewLaunch
    :: DomBuilder t m
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewLaunch
    Vy.Validity{launch = v}
    ValidityWorking{launch = LaunchValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Launch Validity = " <> Vy.showLaunchValidity v
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
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewDistance
    Vy.Validity{distance = v}
    ValidityWorking{distance = DistanceValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Distance Validity = " <> Vy.showDistanceValidity v
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
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewTime
    Vy.Validity{time = v}
    ValidityWorking{time = TimeValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Time Validity = " <> Vy.showTimeValidity v
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

            elClass "div" "notification" $ do
                el "p" $ text "ss best time = the best time ignoring start gates"
                el "p" $ text "gs best time = the best time from the start gate taken"

    return ()

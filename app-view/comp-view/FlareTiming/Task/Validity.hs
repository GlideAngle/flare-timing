module FlareTiming.Task.Validity (viewValidity) where

import Prelude hiding (sum)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy (Validity(..), showLaunchValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    )

launchWorking :: Vy.Validity -> T.Text
launchWorking vy =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{f}{p * n})"
    <> " \\\\\\\\ "
    <> "validity &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> " \\\\\\\\ "
    <> "&= "
    <> (Vy.showLaunchValidity . Vy.launch $ vy)
    <> " \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
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
            (Just vy', Just vw') -> do
                elAttr
                    "a"
                    (("class" =: "button") <> ("onclick" =: launchWorking vy'))
                    (text "Show Working")

                spacer
                viewLaunch vw'
                spacer
                viewDistance vw'
                spacer
                viewTime vw'
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
                        elClass "span" "tag" $ do text "pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "area"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ area)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nominal goal"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalGoal)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "sum of distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ sum)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "minimum distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ minimumDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)

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
                        elClass "span" "tag" $ do text "gs best time"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ gsBestTime)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nominal time"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalTime)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)

    return ()

module FlareTiming.Task.Validity (viewValidity) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    )

launchWorking :: T.Text
launchWorking =
    "katex.render("
    <> "\"\\\\begin{aligned} x &= \\\\min(1, \\\\frac{flying}{present * nominal}) \\\\\\\\ launch &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3 \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
    <> ", {throwOnError: false});"

viewValidity
    :: MonadWidget t m
    => Dynamic t (Maybe ValidityWorking)
    -> m ()
viewValidity v = do
    _ <- dyn $ ffor v (\x ->
        case x of
            Nothing -> text "Loading nominals ..."
            (Just ValidityWorking{launch = LaunchValidityWorking{..}}) -> do
                elClass "div" "field is-grouped is-grouped-multiline" $ do
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "pilots flying"
                            elClass "span" "tag is-info"
                                $ text (T.pack . show $ flying)
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "pilots present"
                            elClass "span" "tag is-success"
                                $ text (T.pack . show $ present)
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal launch"
                            elClass "span" "tag is-primary"
                                $ text (T.pack . show $ nominalLaunch)

                elAttr
                    "a"
                    (("class" =: "button") <> ("onclick" =: launchWorking))
                    (text "Show Working")

                elAttr
                    "div"
                    ("id" =: "launch-working")
                    (text "")

                return ())

    return ()

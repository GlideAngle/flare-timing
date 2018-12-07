module FlareTiming.Task.Validity (viewValidity) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy (Validity(..), showLaunchValidity)
import WireTypes.ValidityWorking (ValidityWorking(..), LaunchValidityWorking(..))

launchWorking :: Vy.Validity -> T.Text
launchWorking vy =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{flying}{present * nominal})"
    <> " \\\\\\\\ "
    <> "launch &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> " \\\\\\\\ "
    <> "&= "
    <> (Vy.showLaunchValidity . Vy.launch $ vy)
    <> " \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
    <> ", {throwOnError: false});"

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
            (Just vy', Just vw') -> viewLaunch vy' vw')

    return ()

viewLaunch
    :: DomBuilder t m
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewLaunch vy ValidityWorking{launch = LaunchValidityWorking{..}} = do
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
        (("class" =: "button") <> ("onclick" =: launchWorking vy))
        (text "Show Working")

    elAttr
        "div"
        ("id" =: "launch-working")
        (text "")

    return ()

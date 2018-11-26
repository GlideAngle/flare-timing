module FlareTiming.Comp (comps, compTask) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack, intercalate)
import Data.Map (union)
import Data.Maybe (listToMaybe)
import Control.Applicative (pure)

import WireTypes.Comp
    (Comp(..), Nominal(..), Task(..), UtcOffset(..), getRaceRawZones)
import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Comms (getNominals)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Comps will be shown here"

compTask
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t Comp
    -> m ()
compTask t _ = do
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate " - " <$> zs

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title

comp
    :: MonadWidget t m
    => Dynamic t [Nominal]
    -> Dynamic t Comp
    -> m ()
comp ns c = do
    let n = listToMaybe <$> ns
    let title = fmap (T.pack . (\Comp{..} -> compName)) c
    let subtitle =
            fmap (T.pack . (\Comp{..} ->
                mconcat [ from
                        , " to "
                        , to
                        , ", "
                        , location
                        ])) c

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title
                    elClass "p" "title is-5" $ do
                        dynText subtitle
                elClass "div" "example" $ do
                    nominal n $ utcOffset <$> c

nominal
    :: MonadWidget t m
    => Dynamic t (Maybe Nominal)
    -> Dynamic t UtcOffset
    -> m ()
nominal n u = do
    _ <- dyn $ ffor n (\x ->
        case x of
            Nothing -> text "Loading nominals ..."
            (Just Nominal{..}) -> do
                elClass "div" "field is-grouped is-grouped-multiline" $ do
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "UTC offset"
                            elClass "span" "tag is-warning" $ do
                                dynText $ showUtcOffset <$> u
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal distance"
                            elClass "span" "tag is-info" $ do
                                text $ T.pack distance
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal time"
                            elClass "span" "tag is-success" $ do
                                text $ T.pack time
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal goal"
                            elClass "span" "tag is-primary"
                                $ text (T.pack . show $ goal))

    return ()

showUtcOffset :: UtcOffset -> T.Text
showUtcOffset UtcOffset{timeZoneMinutes = mins} =
    T.pack $ show mins ++ " mins"

comps :: MonadWidget t m => Dynamic t [Comp] -> m ()
comps cs = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do
            widgetHold loading $ fmap (pure $ viewComps cs) pb
        elClass "div" "spacer" $ return ()

    return ()

viewComps :: MonadWidget t m => Dynamic t [Comp] -> m ()
viewComps cs = do
    ns <- getNominals ()

    _ <-
        elAttr
            "div"
            (union
                ("class" =: "tile is-ancestor")
                ("style" =: "flex-wrap: wrap;")
            ) $ do
        simpleList cs (comp ns)

    return ()

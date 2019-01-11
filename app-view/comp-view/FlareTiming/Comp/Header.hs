module FlareTiming.Comp.Header (compHeader) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Map (union)
import Data.Maybe (listToMaybe)
import Control.Applicative (pure)

import WireTypes.Comp
    ( Comp(..), Nominal(..), UtcOffset(..), ScoreBackTime
    , showMinimumDistance, showScoreBackTime
    )

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Comps will be shown here"

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
                    nominal n (utcOffset <$> c) (scoreBack <$> c)

nominal
    :: MonadWidget t m
    => Dynamic t (Maybe Nominal)
    -> Dynamic t UtcOffset
    -> Dynamic t (Maybe ScoreBackTime)
    -> m ()
nominal n u sb = do
    _ <- dyn $ ffor2 n sb (\x y ->
        case x of
            Nothing -> text "Loading nominals ..."
            Just Nominal{..} -> do
                elClass "div" "field is-grouped is-grouped-multiline" $ do
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "UTC offset"
                            elClass "span" "tag is-warning" $ do
                                dynText $ showUtcOffset <$> u
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "minimum distance"
                            elClass "span" "tag is-black" $ do
                                text . showMinimumDistance $ free
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal distance"
                            elClass "span" "tag is-info" $ do
                                text $ T.pack distance
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal time"
                            elClass "span" "tag is-success" $ do
                                text . T.pack . show $ time
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal goal"
                            elClass "span" "tag is-primary" $ do
                                text . T.pack . show $ goal
                    case y of
                        Nothing -> return ()
                        Just y' ->
                            elClass "div" "control" $ do
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ do text "score-back time"
                                    elClass "span" "tag is-danger" $ do
                                        text . T.pack . showScoreBackTime $ y')

    return ()

showUtcOffset :: UtcOffset -> T.Text
showUtcOffset UtcOffset{timeZoneMinutes = mins} =
    T.pack $ show mins ++ " mins"

compHeader
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Nominal]
    -> m ()
compHeader cs ns = do
    pb <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do
            widgetHold loading $ fmap (pure $ viewComps cs ns) pb
        elClass "div" "spacer" $ return ()

    return ()

viewComps
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Nominal]
    -> m ()
viewComps cs ns = do
    _ <-
        elAttr
            "div"
            (union
                ("class" =: "tile is-ancestor")
                ("style" =: "flex-wrap: wrap;")
            ) $ do
        simpleList cs (comp ns)

    return ()

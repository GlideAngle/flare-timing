module FlareTiming.Comp (getComps, comps, compTask) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack, intercalate)
import Data.Map (union)
import Data.Maybe (listToMaybe)
import Control.Applicative (pure)

import Data.Flight.Types
    (Comp(..), Nominal(..), Task(..), getSpeedSection)
import qualified FlareTiming.Turnpoint as TP (getName)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Comps will be shown here"

compTask
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t Comp
    -> m ()
compTask t _ = do
    let tName = fmap (T.pack . (\Task{..} -> taskName)) t
    let xs = getSpeedSection <$> t
    let zs = (fmap . fmap) (T.pack . TP.getName) xs
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
                    nominal n

nominal
    :: MonadWidget t m
    => Dynamic t (Maybe Nominal)
    -> m ()
nominal n = do
    _ <- dyn $ ffor n (\x ->
        case x of
            Nothing -> text "Loading nominals ..."
            (Just Nominal{..}) -> do
                elClass "div" "field is-grouped is-grouped-multiline" $ do
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

comps :: MonadWidget t m => Dynamic t [Comp] -> m ()
comps cs = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do
            widgetHold loading $ fmap (pure $ viewComps cs) pb
        elClass "div" "spacer" $ return ()

    return ()

getComps
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Comp])
getComps () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/comps"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Comp = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Comp] <- holdDyn [] (pure <$> es)
    return xs

getNominals
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Nominal])
getNominals () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/nominals"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Nominal = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Nominal] <- holdDyn [] (pure <$> es)
    return xs

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

module FlareTiming.Comp.Header (compHeader) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack)
import Data.Map (union)
import Control.Applicative (pure)

import WireTypes.Comp (Comp(..))

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Comps will be shown here"

comp :: MonadWidget t m => Dynamic t Comp -> m ()
comp c = do
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

compHeader :: MonadWidget t m => Dynamic t [Comp] -> m ()
compHeader cs = do
    pb <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do
            widgetHold loading $ fmap (pure $ viewComps cs) pb
        elClass "div" "spacer" $ return ()

    return ()

viewComps :: MonadWidget t m => Dynamic t [Comp] -> m ()
viewComps cs = do
    _ <-
        elAttr
            "div"
            (union
                ("class" =: "tile is-ancestor")
                ("style" =: "flex-wrap: wrap;")
            ) $ do
        simpleList cs comp

    return ()

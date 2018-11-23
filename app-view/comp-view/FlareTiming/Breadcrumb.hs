module FlareTiming.Breadcrumb (crumbTask) where

import Reflex.Dom
import qualified Data.Text as T (pack)

import Data.Flight.Types (Comp(..), Task(..))

crumbTask
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t Comp
    -> m ()
crumbTask t c = do
    let c' = fmap (T.pack . (\Comp{..} -> compName)) c
    let t' = fmap (T.pack . (\Task{..} -> taskName)) t

    elClass "nav" "breadcrumb" $ do
        el "ul" $ do
            el "li" $ el "a" $ dynText c'
            elClass "li" "is-active" $ elAttr "a" ("href" =: "#")$ dynText t'

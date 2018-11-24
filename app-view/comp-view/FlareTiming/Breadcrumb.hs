module FlareTiming.Breadcrumb (crumbTask) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack)

import WireTypes.Comp (Comp(..), Task(..))
import FlareTiming.Events (IxTask(..))

crumbTask
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t Comp
    -> m (Event t IxTask)
crumbTask t c = do
    let c' = fmap (T.pack . (\Comp{..} -> compName)) c
    let t' = fmap (T.pack . (\Task{..} -> taskName)) t

    elClass "nav" "breadcrumb" $ do
        el "ul" $ mdo
            (root, _) <- el' "li" $ el "a" $ dynText c'
            elClass "li" "is-active" $ elAttr "a" ("href" =: "#")$ dynText t'
            return $ (const IxTaskNone) <$> domEvent Click root

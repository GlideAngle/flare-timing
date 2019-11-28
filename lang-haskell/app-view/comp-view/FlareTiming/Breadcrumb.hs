module FlareTiming.Breadcrumb (crumbComp, crumbTask) where

import Data.Function ((&))
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map as Map

import WireTypes.Comp (Comp(..), Task(..))
import FlareTiming.Events (IxTask(..))

hrefParent :: Map.Map T.Text T.Text
hrefParent = "href" =: "http://flaretiming.com"

liParent :: MonadWidget t m => m ()
liParent = el "li" . elAttr "a" hrefParent $ text "Flare Timing"

crumbComp
    :: MonadWidget t m
    => Dynamic t Comp
    -> m ()
crumbComp c = do
    let c' = fmap (T.pack . (\Comp{..} -> compName)) c

    elClass "nav" "breadcrumb" $ do
        el "ul" $ mdo
            liParent
            elClass "li" "is-active" $ elAttr "a" ("href" =: "#")$ dynText c'
            return ()

crumbTask
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Task
    -> Dynamic t Comp
    -> m (Event t IxTask)
crumbTask ixTask t c = do
    let i = ixTask & \case (IxTask ix) -> show ix; IxTaskNone -> "?"
    let c' = fmap (T.pack . (\Comp{..} -> compName)) c
    let t' = fmap (T.pack . (\Task{..} -> "#" ++ i ++ " " ++ taskName)) t

    elClass "nav" "breadcrumb" $ do
        el "ul" $ mdo
            liParent
            (root, _) <- el' "li" $ el "a" $ dynText c'
            elClass "li" "is-active" $ elAttr "a" ("href" =: "#")$ dynText t'
            return $ (const IxTaskNone) <$> domEvent Click root

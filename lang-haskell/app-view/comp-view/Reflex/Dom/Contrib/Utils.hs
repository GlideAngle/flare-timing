module Reflex.Dom.Contrib.Utils where

import Control.Monad.Reader
import Reflex
import Reflex.Dom
import GHCJS.DOM as DOM
import GHCJS.DOM.Types hiding (Text, Event)
import qualified GHCJS.DOM.Window as DOM

alertEvent
    :: (PerformEvent t m, MonadJSM m, MonadJSM (Performable m))
    => (a -> String) -> Event t a -> m ()
alertEvent str e = do
  Just window <- currentWindow
  performEvent_ (DOM.alert window . str <$> e)

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)

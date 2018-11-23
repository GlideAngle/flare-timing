module FlareTiming.Task.ListItem
    ( liTask
    ) where

import Prelude hiding (map)
import qualified Data.Text as T (pack, intercalate)
import Reflex
import Reflex.Dom

import Data.Flight.Types (Task(..), getRaceRawZones)
import qualified FlareTiming.Turnpoint as TP (getName)

liTask
    :: MonadWidget t m
    => Dynamic t Task
    -> m (Event t ())
liTask x = do
    y :: Task <- sample . current $ x
    let jj  = T.pack . taskName $ y
    let xs = getRaceRawZones y
    let zs = TP.getName <$> xs

    (e, _) <-
            el' "li" $ do
                el "a" . text $ jj <> ": " <> T.intercalate " - " zs

    return $ domEvent Click e

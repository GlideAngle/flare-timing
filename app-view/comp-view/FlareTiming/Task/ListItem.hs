module FlareTiming.Task.ListItem (liTask) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (intercalate)

import FlareTiming.Events (IxTask(..))
import WireTypes.Comp (Task(..), getRaceRawZones)
import WireTypes.Route (TaskDistance, showTaskDistance)
import qualified FlareTiming.Turnpoint as TP (getName)

liTask
    :: MonadWidget t m
    => Dynamic t [TaskDistance]
    -> Dynamic t (IxTask, Task)
    -> m (Event t ())
liTask ds' x' = do
    ds <- sample . current $ ds'
    (ix, x) <- sample . current $ x'
    case ix of
        IxTaskNone -> return never
        IxTask i -> do
            let zs = getRaceRawZones x
            let ns = TP.getName <$> zs
            let d = case drop (i - 1) ds of
                        dTask : _ -> showTaskDistance dTask
                        _ -> ""

            (e, _) <-
                    el' "li" $ do
                        text $ d <> " "
                        el "a" . text
                            $ T.intercalate " - " ns

            return $ domEvent Click e

module FlareTiming.Comp.Tasks (taskList) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack, intercalate)

import FlareTiming.Events (IxTask(..))
import WireTypes.Comp (Task(..), getRaceRawZones)
import WireTypes.Route (TaskDistance(..), showTaskDistance)
import qualified FlareTiming.Turnpoint as TP (getName)

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [TaskDistance]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
taskList ds' xs = do
    ev <- dyn $ ffor ds' (\ds -> do
            if null ds
                then
                    elClass "article" "notification is-warning" $
                        el "p" $ text "No distances are available for tasks."
                else
                    return ()

            let ixs = zip (IxTask <$> [1..]) <$> xs
            ys <- elClass "ol" "ol-tasks" $ simpleList ixs (liTask ds)
            return $ switchDyn (listToIxTask <$> ys))

    switchHold never ev

liTask
    :: MonadWidget t m
    => [TaskDistance]
    -> Dynamic t (IxTask, Task)
    -> m (Event t ())
liTask ds x' = do
    (ix, x@Task{taskName}) <- sample . current $ x'
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
                        el "a" . text
                            $ T.intercalate " - " ns
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag is-white" $ text (T.pack taskName)
                            elClass "span" "tag is-white" $ text d

            return $ domEvent Click e

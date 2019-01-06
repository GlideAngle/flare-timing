module FlareTiming.Comp.Tasks (taskList) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack, intercalate)
import Text.Printf (printf)

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
taskList lens xs = do
    let ixs = zip (IxTask <$> [1..]) <$> xs
    ys <- elClass "ol" "ol-tasks" $ simpleList ixs (liTask lens)
    return $ switchDyn (listToIxTask <$> ys)

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
                        el "a" . text
                            $ T.intercalate " - " ns
                        text " "
                        text $ d

            return $ domEvent Click e

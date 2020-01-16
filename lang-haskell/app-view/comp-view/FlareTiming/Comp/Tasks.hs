module FlareTiming.Comp.Tasks (taskList) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack, intercalate)
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
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
taskList ds' stats' xs = do
    ev <- dyn $ ffor2 ds' stats' (\ds stats -> do
            if null ds
                then
                    elClass "article" "notification is-warning" $
                        el "p" $ text "No distances are available for tasks."
                else
                    return ()

            let ixs = zip (IxTask <$> [1..]) <$> xs
            ys <- elClass "ol" "ol-tasks" $ simpleList ixs (liTask ds stats)
            return $ switchDyn (listToIxTask <$> ys))

    switchHold never ev

liTask
    :: MonadWidget t m
    => [TaskDistance]
    -> [Maybe (Double, Double)]
    -> Dynamic t (IxTask, Task)
    -> m (Event t ())
liTask ds stats x' = do
    (ix, x@Task{taskName}) <- sample . current $ x'
    case ix of
        IxTaskNone -> return never
        IxTask i -> do
            let zs = getRaceRawZones x
            let ns = TP.getName <$> zs
            let d = case drop (i - 1) ds of
                        dTask : _ -> showTaskDistance dTask
                        _ -> ""

            let s = case drop (i - 1) stats of
                        Just (m, sd) : _ -> printf "%+03.1f ± %03.1f" m sd
                        _ -> ""

            (e, _) <-
                    elAttr' "li" ("style" =: "margin: 1em 0") $ do
                        elClass "div" "field is-grouped is-grouped-multiline" $ do
                            elClass "div" "control" $
                                el "a" . text
                                    $ T.intercalate " - " ns
                            elClass "div" "control" $
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ text (T.pack taskName)
                                    elClass "span" "tag is-black" $ text d
                            elClass "div" "control" $
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ text "Δ"
                                    elClass "span" "tag is-warning" $ text (T.pack s)

            return $ domEvent Click e

module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import qualified Data.Text as T (pack)
import Reflex
import Reflex.Dom

import Data.Flight.Types (Comp(..), Task(..))
import FlareTiming.Comp (comps, compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms (getTasks, getComps)
import FlareTiming.Map (map)
import FlareTiming.Task.ListItem (liTask)
import FlareTiming.Comp.Tab (CompTab(..), tabsComp)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- widgetHold loading $ fmap view pb
        elClass "div" "spacer" $ return ()

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [Task]
    -> m (Event t IxTask)
taskList xs = do
    ys <- el "ul" $ simpleList xs liTask
    return $ switchDyn (listToIxTask <$> ys)

compDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
compDetail cs xs = do
    comps cs
    tab <- tabsComp

    e <- widgetHold (taskList xs) $
            (\case
                CompTabTask -> taskList xs
                CompTabPilot -> do
                    text "pilots"
                    return never)
            <$> tab

    return $ switchDyn e

taskDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t Task
    -> m (Event t IxTask)
taskDetail cs x = do
    _ <- simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tab <- tabsTask

    _ <- widgetHold (text "score") $
            (\case
                TaskTabScore -> text "score"
                TaskTabTask -> tableTurnpoints x
                TaskTabMap -> do y <- sample . current $ x; map y
                TaskTabAbsent -> tableAbsent x)
            <$> tab

    return $ switchDyn (leftmost <$> es)

view :: MonadWidget t m => () -> m ()
view () = do
    cs <- getComps ()
    xs <- getTasks ()

    el "div" $ mdo

        deIx <- widgetHold (compDetail cs xs) $
                    (\ix -> case ix of
                        IxTaskNone -> compDetail cs xs
                        IxTask ii -> taskDetail cs $ (!! (ii - 1)) <$> xs)
                    <$> eIx

        let eIx = switchDyn deIx

        dIx <- holdDyn IxTaskNone . leftmost $
            [ eIx
            ]

        let dText =
                (\case IxTask ii -> T.pack . show $ ii; IxTaskNone -> "None")
                <$> dIx

        el "div" $ dynText dText

    return ()

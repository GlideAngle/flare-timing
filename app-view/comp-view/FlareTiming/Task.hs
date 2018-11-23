module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import qualified Data.Text as T (pack, intercalate)
import Reflex
import Reflex.Dom

import Data.Flight.Types
    ( Comp(..), Task(..), Zones(..), RawZone(..), SpeedSection
    , getSpeedSection
    )
import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Comp (comps, getComps, compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

task
    :: MonadWidget t m
    => Dynamic t Task
    -> m (Event t ())
task x = do
    y :: Task <- sample . current $ x
    let jj  = T.pack . taskName $ y
    let xs = getSpeedSection y
    let zs = T.pack . TP.getName <$> xs

    (e, _) <-
            el' "li" $ do
                el "a" . text $ jj <> ": " <> T.intercalate " - " zs

    return $ domEvent Click e

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- widgetHold loading $ fmap view pb
        elClass "div" "spacer" $ return ()

getTasks :: MonadWidget t m => () -> m (Dynamic t [Task])
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
taskList cs xs = do
    comps cs
    elClass "h3" "subtitle is-3" $ text "Tasks"
    ys <- el "ul" $ simpleList xs task
    return $ switchDyn (listToIxTask <$> ys)

tabs
    :: MonadWidget t m
    => m ()
tabs = do
    elClass "div" "tabs" $
        el "ul" $ do
            elClass "li" "is-active" $ el "a" (text "Task")
            el "li" $ el "a" (text "Scores")

taskDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t Task
    -> m (Event t IxTask)
taskDetail cs x = do
    simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tabs
    x' <- sample . current $ x
    map x'
    return $ switchDyn (leftmost <$> es)

view :: MonadWidget t m => () -> m ()
view () = do
    cs <- getComps ()
    xs <- getTasks ()

    el "div" $ mdo

            deIx <- widgetHold (taskList cs xs) . leftmost $
                [
                    (\ix -> case ix of
                        IxTaskNone -> taskList cs xs
                        IxTask ii -> taskDetail cs $ (!! (ii - 1)) <$> xs)
                    <$> eIx
                ]

            let eIx = switchDyn deIx

            dIx <- holdDyn IxTaskNone . leftmost $
                [ eIx
                ]

            let dText =
                    (\case IxTask ii -> T.pack . show $ ii; IxTaskNone -> "None")
                    <$> dIx

            el "div" $ dynText dText

    return ()

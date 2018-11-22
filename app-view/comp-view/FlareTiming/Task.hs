module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import qualified Data.Text as T (pack, intercalate)
import Reflex
import Reflex.Dom

import Data.Flight.Types (Task(..), Zones(..), RawZone(..), SpeedSection)
import qualified FlareTiming.Turnpoint as TP (getName)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

data IxTask
    = IxTask Int
    | IxTaskNone

getSpeedSection :: Task -> [RawZone]
getSpeedSection (Task _ Zones{raw = tps} ss) =
    speedSectionOnly ss tps
    where
        speedSectionOnly :: SpeedSection -> [RawZone] -> [RawZone]
        speedSectionOnly Nothing xs =
            xs
        speedSectionOnly (Just (start, end)) xs =
            take (end' - start' + 1) $ drop (start' - 1) xs
            where
                start' = fromInteger start
                end' = fromInteger end

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
        _ <- widgetHold loading $ fmap getTasks pb
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
    elClass "h3" "subtitle is-3" $ text "Tasks"
    ys <- el "ul" $ simpleList xs task
    return $ switchDyn (listToIxTask <$> ys)

taskDetail
    :: MonadWidget t m
    => Dynamic t Task
    -> m (Event t IxTask)
taskDetail x = do
    elClass "h3" "subtitle is-3" $ text "Task"
    y <- el "ul" $ task x
    return $ fmap (const IxTaskNone) y

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Task] <- holdDyn [] es

    el "div" $ mdo
            eShowAll <- el "div" $ button "Show All Tasks"

            deIx <- widgetHold (taskList xs) . leftmost $
                [ taskList xs <$ eShowAll
                ,
                    (\ix -> case ix of
                        IxTaskNone -> taskList xs
                        IxTask ii -> taskDetail $ (!! (ii - 1)) <$> xs)
                    <$> eIx
                ]

            let eIx = switchDyn deIx

            dIx <- holdDyn IxTaskNone . leftmost $
                [ eIx
                , IxTaskNone <$ eShowAll
                ]

            let dText =
                    (\case IxTask ii -> T.pack . show $ ii; IxTaskNone -> "None")
                    <$> dIx

            el "div" $ dynText dText

    return ()

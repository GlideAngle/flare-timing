module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import qualified Data.Text as T (pack, intercalate)
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Utils

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
    :: forall t (m :: * -> *). MonadWidget t m
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

tasks :: MonadWidget t m => Maybe Int -> m ()
tasks iTask = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        iTask <- widgetHold loading $ fmap getTasks pb
        elClass "div" "spacer" $ return ()

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [Task]
    -> m(Event t IxTask)
taskList xs = do
    ys <- do
            elClass "h3" "subtitle is-3" $ text "Tasks"
            el "ul" $ simpleList xs task

    return $ switchDyn (listToIxTask <$> ys)

taskDetail
    :: MonadWidget t m
    => Dynamic t [Task]
    -> m(Event t IxTask)
taskDetail _ = do
    elClass "h3" "subtitle is-3" $ text "Task"
    return never

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Task] <- holdDyn [] es

    el "div" $ do
        eSwitch <- el "div" $ button "Switch"
        dToggle <- toggle True eSwitch

        let eShow1 = ffilter id . updated $ dToggle
        let eShow2 = ffilter not . updated $ dToggle

        deText <- widgetHold (taskList xs) . leftmost $
            [ taskList xs <$ eShow1
            , taskDetail xs <$ eShow2
            ]

        let eText = switchDyn deText

        dIx <- holdDyn IxTaskNone . leftmost $
            [ eText
            , IxTaskNone <$ eSwitch
            ]

        let dText =
                (\case IxTask ii -> T.pack . show $ ii; IxTaskNone -> "None")
                <$> dIx

        el "div" $ dynText dText

    return ()

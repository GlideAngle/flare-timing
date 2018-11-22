module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import Reflex.Dom
    ( MonadWidget, Event, Dynamic, XhrRequest(..), EventName(Click)
    , (=:)
    , def
    , holdDyn
    , foldDyn
    , sample
    , current
    , updated
    , widgetHold
    , widgetHold_
    , elAttr
    , elClass
    , elDynClass'
    , el
    , el'
    , text
    , dynText
    , simpleList
    , listWithKey
    , getPostBuild
    , fmapMaybe
    , performRequestAsync
    , decodeXhrResponse
    , leftmost
    , domEvent
    , toggle
    )
import qualified Data.Text as T (Text, pack, intercalate)
import Data.Map (Map, union, fromList)

import Data.Flight.Types (Task(..), Zones(..), RawZone(..), SpeedSection)
import qualified FlareTiming.Turnpoint as TP (getName)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

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
    => Int
    -> Dynamic t Task
    -> m (Event t Int)
task ii x = do
    let jj = T.pack $ show ii
    let xs = getSpeedSection <$> x
    let tps = (fmap . fmap) (T.pack . TP.getName) xs
    zs <- sample . current $ tps

    (e, _) <-
            el' "li" $ do
                el "a" . text $ "Task " <> jj <> ": " <> T.intercalate " - " zs
    return $ const ii <$> domEvent Click e

tasks :: MonadWidget t m => Maybe Int -> m ()
tasks iTask = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        iTask <- widgetHold loading $ fmap getTasks pb
        elClass "div" "spacer" $ return ()

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp

    xs
        :: Dynamic t (Map Int Task)
        <- holdDyn (fromList []) $ (fromList . (zip [1..])) <$> es

    elClass "h3" "subtitle is-3" $ text "Tasks"
    el "ul" $ listWithKey xs task

    return ()

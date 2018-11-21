module FlareTiming.Task (tasks) where

import Prelude hiding (map)
import Reflex.Dom
    ( MonadWidget, Event, Dynamic, XhrRequest(..), EventName(Click)
    , (=:)
    , def
    , holdDyn
    , sample
    , current
    , widgetHold
    , elAttr
    , elClass
    , elDynClass'
    , el
    , text
    , dynText
    , simpleList
    , getPostBuild
    , fmapMaybe
    , performRequestAsync
    , decodeXhrResponse
    , leftmost
    , domEvent
    , toggle
    )
import qualified Data.Text as T (pack)
import Data.Map (union)
import Data.List (intercalate)

import Data.Flight.Types (Task(..), Zones(..), RawZone(..), SpeedSection)
import FlareTiming.Map (map)
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

hyphenate :: [RawZone] -> String
hyphenate xs =
    intercalate " - " $ fmap TP.getName xs

task
    :: forall t (m :: * -> *). MonadWidget t m
    => Dynamic t (Int, Task)
    -> m (Dynamic t Bool)
task ix = do
    i :: String <- sample $ current $ fmap (show . fst) ix
    let x :: Dynamic t Task = fmap snd ix
    let xs = fmap getSpeedSection x
    let subtitle = fmap (T.pack . (\s -> "#" ++ i ++ " - " ++ s) . hyphenate) xs

    y :: Task <- sample $ current x

    elClass "div" "tile" $
        elClass "div" "tile is-parent" $
            elClass "div" "tile is-child box" $ do
                map y
                elClass "p" "" $ mdo
                    (a, _) <- elDynClass' "a" c $ dynText subtitle
                    e <- toggle False $ domEvent Click a
                    c <- return $ (\case False -> "button"; True -> "button is-success") <$> e
                    return e

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do widgetHold loading $ fmap getTasks pb
        elClass "div" "spacer" $ return ()

    return ()

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Task] <- holdDyn [] es
    let ys :: Dynamic t [(Int, Task)] = fmap (zip [1 .. ]) xs

    _ <-
        elAttr
            "div"
            (union
                ("class" =: "tile is-ancestor")
                ("style" =: "flex-wrap: wrap;")
            ) $ do
        simpleList ys task

    return ()

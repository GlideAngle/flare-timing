module FlareTiming.Task (tasks) where

import qualified Data.Text as T (pack)
import Data.List (nub, sort)
import Reflex
import Reflex.Dom

import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms (getTasks, getComps, getPilots)
import FlareTiming.Comp.Detail (compDetail)
import FlareTiming.Task.Detail (taskDetail)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- widgetHold loading $ fmap view pb
        elClass "div" "spacer" $ return ()

view :: MonadWidget t m => () -> m ()
view () = do
    cs <- getComps ()
    xs <- getTasks ()
    pss <- getPilots ()
    let ps = sort . nub . concat <$> pss

    el "div" $ mdo

        deIx <- widgetHold (compDetail cs ps xs) $
                    (\ix -> case ix of
                        IxTaskNone -> compDetail cs ps xs
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

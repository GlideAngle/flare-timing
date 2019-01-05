module FlareTiming.Task (tasks) where

import Reflex
import Reflex.Dom
import Control.Monad (join)

import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms
    (getTasks, getTaskLengths, getComps, getValidity, getAllocation)
import FlareTiming.Comp.Detail (compDetail)
import FlareTiming.Task.Detail (taskDetail)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container is-size-7" $ do
        _ <- widgetHold loading $ fmap view pb
        elClass "div" "spacer" $ return ()

view :: MonadWidget t m => () -> m ()
view () = do
    pb :: Event t () <- getPostBuild
    ls <- holdDyn [] =<< getTaskLengths pb
    xs <- holdDyn [] =<< delay 1 =<< getTasks pb
    cs <- getComps ()
    vs <- getValidity ()
    as <- getAllocation ()

    el "div" $ mdo

        let eIx = switchDyn deIx

        deIx <- widgetHold (compDetail ls cs xs) $
                    (\ix -> case ix of
                        IxTaskNone -> compDetail ls cs xs
                        (IxTask ii) -> do
                            taskDetail
                                ix
                                cs
                                ((!! (ii - 1)) <$> xs)
                                (join . nth (ii - 1) <$> vs)
                                (join . nth (ii - 1) <$> as))
                    <$> eIx

        return ()

    return ()

nth :: Int -> [a] -> Maybe a
nth ii xs =
    case drop ii xs of
        [] -> Nothing
        x : _ -> Just x

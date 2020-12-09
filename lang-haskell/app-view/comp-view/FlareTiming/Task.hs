module FlareTiming.Task (tasks) where

import Reflex
import Reflex.Dom
import Control.Monad (join)

import FlareTiming.Events (IxTask(..))
import FlareTiming.Comms
    ( getTasks, getTaskLengths, getStatsPointDiff, getComps, getNominals
    , getValidity, getAltValidity
    , getAllocation
    )
import FlareTiming.Comp.Detail (compDetail)
import FlareTiming.Task.Detail (taskDetail)

loading :: MonadWidget t m => m ()
loading = el "li" $ text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = do
    pb <- getPostBuild
    elClass "div" "spacer" $ return ()
    elClass "div" "container is-size-7" $ do
        _ <- widgetHold loading $ fmap view pb
        elClass "div" "spacer" $ return ()

view :: MonadWidget t m => () -> m ()
view () = do
    pb <- getPostBuild
    ns <- holdDyn [] . fmap pure =<< getNominals pb
    ls <- holdDyn [] =<< getTaskLengths pb
    xs <- holdDyn [] =<< getTasks pb
    stats <- holdDyn [] =<< getStatsPointDiff pb

    let bothLists = zipDynWith (,) ls xs
    let xsNotNull = ffor xs (\xs' -> not $ null xs')

    lxs <-
            holdDyn ([], [])
            $ tag (current bothLists)
            $ gate (current xsNotNull)
            $ leftmost
                [ () <$ updated ls
                , () <$ updated xs
                ]

    let (ls', xs') = splitDynPure lxs

    cs <- holdDyn [] . fmap pure =<< getComps pb
    vs <- holdDyn [] =<< getValidity pb
    nvs <- holdDyn [] =<< getAltValidity pb
    as <- holdDyn [] =<< getAllocation pb

    el "div" $ mdo

        let eIx = switchDyn deIx

        deIx <- widgetHold (compDetail ls' cs ns stats xs') $
                    (\ix -> case ix of
                        IxTaskNone -> compDetail ls' cs ns stats xs'
                        (IxTask ii) -> do
                            taskDetail
                                ix
                                (head <$> cs)
                                (head <$> ns)
                                ((!! (ii - 1)) <$> xs)
                                (join . nth (ii - 1) <$> vs)
                                (join . nth (ii - 1) <$> nvs)
                                (join . nth (ii - 1) <$> as))
                    <$> eIx

        return ()

    return ()

nth :: Int -> [a] -> Maybe a
nth ii xs =
    case drop ii xs of
        [] -> Nothing
        x : _ -> Just x

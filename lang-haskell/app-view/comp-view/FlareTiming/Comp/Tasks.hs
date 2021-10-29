module FlareTiming.Comp.Tasks (taskList) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack, intercalate)
import Text.Printf (printf)

import FlareTiming.Events (IxTask(..))
import WireTypes.Comp (Task(..), TaskStop(..), getRaceRawZones)
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
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
taskList ds' diffFtFs diffFtAs diffAsFs xs = do
    ev <- dyn $ ffor ds' (\ds -> do
            if null ds
                then
                    elClass "article" "notification is-warning" $
                        el "p" $ text "No distances are available for tasks."
                else
                    return ()

            let ixs = zip (IxTask <$> [1..]) <$> xs
            ys <-
                elClass "table" "table is-striped" $ do
                    el "thead" $ do
                        el "tr" $ do
                            elAttr "th" ("colspan" =: "6") $ text ""
                            elAttr "th" ("colspan" =: "6" <> "class" =: "has-text-centered has-text-weight-bold is-light")
                                $ text "3-Way (Ft,Fs,As) Comparison"

                        el "tr" $ do
                            elAttr "th" ("colspan" =: "6") $ text ""
                            elAttr "th" ("colspan" =: "2" <> "class" =: "has-text-centered is-light")
                                $ text "Ft with Fs"
                            elAttr "th" ("colspan" =: "2" <> "class" =: "has-text-centered")
                                $ text "Ft with As"
                            elAttr "th" ("colspan" =: "2" <> "class" =: "has-text-centered is-light")
                                $ text "As with Fs"

                        el "tr" $ do
                            el "th" $ text "#"
                            elClass "th" "th-task-name" $ text "Name"
                            elClass "th" "th-task-tps" $ text "Turnpoints"
                            elClass "th" "th-task-dist" $ text "Distance"

                            elClass "th" "th-task-stopped" $ text "Stopped"
                            elClass "th" "th-task-cancelled" $ text "Cancelled"

                            elClass "th" "th-task-stats-mean" $ text "Δ Mean"
                            elClass "th" "th-task-stats-stddev" $ text "± Std Dev"

                            elClass "th" "th-task-stats-mean" $ text "Δ Mean"
                            elClass "th" "th-task-stats-stddev" $ text "± Std Dev"

                            elClass "th" "th-task-stats-mean" $ text "Δ Mean"
                            elClass "th" "th-task-stats-stddev" $ text "± Std Dev"

                    rows <- el "tbody" $ simpleList ixs (rowTask ds diffFtFs diffFtAs diffAsFs)

                    let tdFoot = elAttr "td" ("colspan" =: "14")
                    let foot = el "tr" . tdFoot . text

                    el "tfoot" $ do
                        foot "Δ The mean of the difference in total points awarded compared to the expected total points."
                        foot "± The standard deviation of the same."
                        foot "Flare Timing (Ft), Flight System (Fs) and airScore (As) are compared pairwise in the difference of total points per pilot."

                    return rows

            return $ switchDyn (listToIxTask <$> ys))

    switchHold never ev

data ShowDiff =
    ShowDiff
        { diffMean :: T.Text
        , diffStdDev :: T.Text
        , classMean :: T.Text
        , classStdDev :: T.Text
        }

rowTask
    :: MonadWidget t m
    => [TaskDistance]
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t [Maybe (Double, Double)]
    -> Dynamic t (IxTask, Task)
    -> m (Event t ())
rowTask ds diffFtFs diffFtAs diffAsFs x' = do
    let mkDiff i diffs =
            case drop (i - 1) diffs of
                Just (m, sd) : _ ->
                    let (m', sd') = (abs m, abs sd) in
                    ShowDiff
                        { diffMean = T.pack $ printf "%+03.1f" m
                        , diffStdDev = T.pack $ printf "%03.1f" sd'
                        , classMean =
                              if | m' < 2 -> "has-text-success"
                                 | m' < 8 -> "has-text-warning"
                                 | otherwise -> "has-text-danger"
                        , classStdDev =
                              if | sd' < 2 -> "has-text-success"
                                 | sd' < 8 -> "has-text-warning"
                                 | otherwise -> "has-text-danger"
                        }
                _ -> ShowDiff "" "" "has-text-danger" "has-text-danger"

    ev <- dyn $ ffor x' (\(ix, x@Task{taskName, stopped, cancelled}) ->
                let isStopped = case stopped of Just TaskStop{} -> True; _ -> False in

                case ix of
                    IxTaskNone -> return never
                    IxTask i -> do
                        let zs = getRaceRawZones x
                        let ns = TP.getName <$> zs
                        let d = case drop (i - 1) ds of
                                    dTask : _ -> showTaskDistance dTask
                                    _ -> ""

                        let showFtFs = ffor diffFtFs $ mkDiff i
                        let showFtAs = ffor diffFtAs $ mkDiff i
                        let showAsFs = ffor diffAsFs $ mkDiff i

                        (e, _) <-
                                el' "tr" $ do
                                    el "td" . text . T.pack $ printf "%d" i
                                    elClass "td" "td-task-name" $ text (T.pack taskName)
                                    elClass "td" "td-task-tps" $
                                        el "a" . text $ T.intercalate "-" ns
                                    elClass "td" "td-task-dist" $ text d

                                    elClass "td" "td-task-stopped" . text
                                        $ if isStopped then "STOPPED" else ""
                                    elClass "td" "td-task-cancelled" . text
                                        $ if cancelled then "CANCELLED" else ""

                                    elClass "td" "td-task-stats-mean" $
                                        elDynClass "span" (classMean <$> showFtFs) . dynText
                                            $ diffMean <$> showFtFs
                                    elClass "td" "td-task-stats-stddev" $
                                        elDynClass "span" (classStdDev <$> showFtFs) . dynText
                                            $ diffStdDev <$> showFtFs

                                    elClass "td" "td-task-stats-mean" $
                                        elDynClass "span" (classMean <$> showFtAs) . dynText
                                            $ diffMean <$> showFtAs
                                    elClass "td" "td-task-stats-stddev" $
                                        elDynClass "span" (classStdDev <$> showFtAs) . dynText
                                            $ diffStdDev <$> showFtAs

                                    elClass "td" "td-task-stats-mean" $
                                        elDynClass "span" (classMean <$> showAsFs) . dynText
                                            $ diffMean <$> showAsFs
                                    elClass "td" "td-task-stats-stddev" $
                                        elDynClass "span" (classStdDev <$> showAsFs) . dynText
                                            $ diffStdDev <$> showAsFs

                        return $ domEvent Click e)
    switchHold never ev

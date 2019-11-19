module FlareTiming.Task.Turnpoints (tableTask) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Point (StartGate(..))
import WireTypes.Comp
    (Task(..), SpeedSection, UtcOffset(..), OpenClose(..)
    , getAllRawZones, getSpeedSection, getOpenClose, getStartGates
    , getGoalShape, getEssShape, getOpenShape
    )
import WireTypes.Route (TaskDistance(..), TaskLegs(..), showTaskDistance)
import WireTypes.Zone (RawZone(..))
import WireTypes.ZoneKind (Shape(..), showShape)
import qualified FlareTiming.Turnpoint as TP
import FlareTiming.Time (showT, timeZone)

zero :: TaskDistance
zero = TaskDistance 0

unknownLegs :: [TaskDistance]
unknownLegs = repeat zero

openPad :: [TaskDistance] -> [TaskDistance]
openPad xs = TaskDistance 0 : xs

speedPad :: SpeedSection -> [TaskDistance] -> [TaskDistance]
speedPad Nothing xs = xs
speedPad (Just (start, _)) xs =
    prolog <> xs <> unknownLegs
    where
        -- NOTE: The speed section uses 1-based indexing and the legs are
        -- between turnpoints 1-2, 2-3, etc.
        start' = fromIntegral start
        prolog = take (max 1 $ start' - 1) $ repeat zero

tableTask
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTask utcOffset x taskLegs = do
    tz <- sample . current $ timeZone <$> utcOffset
    gs <- sample . current $ getStartGates <$> x

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-vertical is-7" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    elClass "article" "tile is-child box" $ do
                        elClass "p" "title" $ text "Turn Points"
                        elClass "div" "content" $ do
                            tableTurnPoints x taskLegs
                            return ()

                    elClass "article" "tile is-child box" $ do
                        elClass "p" "title" $ text "Turn Radii"
                        elClass "div" "content" $ do
                            tableTurnRadii x taskLegs
                            return ()

        elClass "div" "tile is-vertical is-5" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    if null gs
                        then
                            elClass "article" "tile is-child notification is-warning" $ do
                                elClass "p" "title" $ text "Start Gates"
                                el "p" $ text "There are no start gates."
                        else
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Start Gates"
                                elClass "div" "content" $ do
                                    tableStartGates tz gs
                                    return ()

                    elClass "article" "tile is-child box" $ do
                        elClass "p" "title" $ text "Time Windows"
                        elClass "div" "content" $ do
                            tableWindows tz x
                            return ()

                    elClass "article" "tile is-child box" $ do
                        elClass "p" "title" $ text "From Launch"
                        elClass "div" "content" $ do
                            tableTurnLaunch x taskLegs
                            return ()

                    elClass "article" "tile is-child box" $ do
                        elClass "p" "title" $ text "To Goal"
                        elClass "div" "content" $ do
                            tableTurnGoal x taskLegs
                            return ()

tableWindows
    :: MonadWidget t m
    => TimeZone
    -> Dynamic t Task
    -> m ()
tableWindows tz x = do
    let zs = getAllRawZones <$> x
    let ss = getSpeedSection <$> x
    let oc = (\case [t] -> repeat t; ts -> ts) . getOpenClose <$> x

    len <- sample . current $ fromIntegral . length <$> zs
    let ys = ffor2 oc zs $ zip

    _ <- elClass "table" "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-open" $ text "Open"
                    elClass "th" "th-tp-close" $ text "Close"

            el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (rowWindow tz len ss)

    return ()

tableTurnPoints
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnPoints x taskLegs = do
    let zs = getAllRawZones <$> x
    let ess = getEssShape <$> x
    let goal = getGoalShape <$> x
    let open = getOpenShape <$> x
    let ss = getSpeedSection <$> x

    len <- sample . current $ fromIntegral . length <$> zs

    (legs', legsSum', flipSum') <- sample . current $ ffor ss (\ss' ->
            let pad = case ss' of Just _ -> speedPad ss'; _ -> openPad
                pl = (maybe unknownLegs (pad . legs)) <$> taskLegs
                ps = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
                pf = (maybe unknownLegs ((<> [zero]) . flipSum)) <$> taskLegs
            in (pl, ps, pf))

    let dd = ffor3 legs' legsSum' flipSum' $ zipWith3 (,,)
    let ys = ffor2 dd zs $ zip

    _ <- elClass "table" "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-lat" $ text "Latitude"
                    elClass "th" "th-tp-lng" $ text "Longitude"
                    elClass "th" "th-tp-altitude" $ text "Altitude"

            _ <- el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (rowTurnPoint len ss)

            let tr = el "tr" . elAttr "td" ("colspan" =: "5")
            el "tfoot" $ do
                dyn_ . ffor2 goal open $ (\g o ->
                    case (g, o) of
                        (Just _, Nothing) -> do
                            tr . dynText $ goalFootnote <$> goal
                            tr $ text "† Start of the speed section"
                            tr . dynText $ essFootnote <$> ess
                        (Nothing, Just _) -> tr . dynText $ openFootnote <$> open
                        (Just _, Just _) -> return ()
                        (Nothing, Nothing) -> return ())

    return ()

tableTurnRadii
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnRadii x taskLegs = do
    let zs = getAllRawZones <$> x
    let ss = getSpeedSection <$> x

    len <- sample . current $ fromIntegral . length <$> zs

    (legs', legsSum', flipSum') <- sample . current $ ffor ss (\ss' ->
            let pad = case ss' of Just _ -> speedPad ss'; _ -> openPad
                pl = (maybe unknownLegs (pad . legs)) <$> taskLegs
                ps = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
                pf = (maybe unknownLegs ((<> [zero]) . flipSum)) <$> taskLegs
            in (pl, ps, pf))

    let dd = ffor3 legs' legsSum' flipSum' $ zipWith3 (,,)
    let ys = ffor2 dd zs $ zip

    _ <- elClass "table" "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-radius" $ text "Radius"
                    elClass "th" "th-tp-give" $ text "Give In §"
                    elClass "th" "th-tp-give" $ text "Give Out §"

            _ <- el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (rowTurnRadius len ss)

            let tr = el "tr" . elAttr "td" ("colspan" =: "5")
            el "tfoot" $ do
                tr $ text " § Give in the radius once the tolerance has been applied"
                tr $ text " ↕ The leg distance between turnpoints"

    return ()

tableTurnLaunch
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnLaunch x taskLegs = do
    let zs = getAllRawZones <$> x
    let ss = getSpeedSection <$> x

    len <- sample . current $ fromIntegral . length <$> zs

    (legs', legsSum', flipSum') <- sample . current $ ffor ss (\ss' ->
            let pad = case ss' of Just _ -> speedPad ss'; _ -> openPad
                pl = (maybe unknownLegs (pad . legs)) <$> taskLegs
                ps = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
                pf = (maybe unknownLegs ((<> [zero]) . flipSum)) <$> taskLegs
            in (pl, ps, pf))

    let dd = ffor3 legs' legsSum' flipSum' $ zipWith3 (,,)
    let ys = ffor2 dd zs $ zip

    _ <- elClass "table" "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-distance-task" $ text "Launch"

            _ <- el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (rowTurnLaunch len ss)

            let tr = el "tr" . elAttr "td" ("colspan" =: "3")
            el "tfoot" $ do
                tr $ text " ↑  The distance from the turnpoint back to launch"

    return ()

tableTurnGoal
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnGoal x taskLegs = do
    let zs = getAllRawZones <$> x
    let ss = getSpeedSection <$> x

    len <- sample . current $ fromIntegral . length <$> zs

    (legs', legsSum', flipSum') <- sample . current $ ffor ss (\ss' ->
            let pad = case ss' of Just _ -> speedPad ss'; _ -> openPad
                pl = (maybe unknownLegs (pad . legs)) <$> taskLegs
                ps = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
                pf = (maybe unknownLegs ((<> [zero]) . flipSum)) <$> taskLegs
            in (pl, ps, pf))

    let dd = ffor3 legs' legsSum' flipSum' $ zipWith3 (,,)
    let ys = ffor2 dd zs $ zip

    _ <- elClass "table" "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-distance-task" $ text "Goal"

            _ <- el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (rowTurnGoal len ss)

            let tr = el "tr" . elAttr "td" ("colspan" =: "3")
            el "tfoot" $ do
                tr $ text " ↓  The distance from the turnpoint forward to goal"

    return ()

essFootnote :: Maybe Shape -> T.Text
essFootnote Nothing = "‡ End of the speed section"
essFootnote (Just s) = "‡ End of the speed section is a " <> (T.pack . showShape $ s)

goalFootnote :: Maybe Shape -> T.Text
goalFootnote (Just s) = "* Goal is a " <> (T.pack . showShape $ s)
goalFootnote Nothing = ""

openFootnote :: Maybe Shape -> T.Text
openFootnote (Just s@Vector{}) = "* Open distance on a heading " <> (T.pack . showShape $ s)
openFootnote (Just Star{}) = "* Open distance in any direction"
openFootnote _ = ""

tableStartGates
    :: MonadWidget t m
    => TimeZone
    -> [StartGate]
    -> m ()
tableStartGates tz gs = do
    let rowNumbers = T.pack . show <$> ([1..] :: [Int])

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    el "th" $ text "Open"
            el "tbody" $ do
                sequence $ zipWith (rowStartGate tz) gs rowNumbers

    return ()

rowStartGate
    :: MonadWidget t m
    => TimeZone
    -> StartGate
    -> T.Text
    -> m ()
rowStartGate tz sg ix = do
    el "tr" $ do
        el "td" $ text ix
        el "td" . text $ showStartGate tz sg

rowWindow
    :: MonadWidget t m
    => TimeZone
    -> Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, (OpenClose, RawZone))
    -> m ()
rowWindow tz len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let x = snd <$> iz
    let oc = fst <$> x
    let z = snd <$> x

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-open" . dynText $ showOpen tz <$> oc
        elClass "td" "td-tp-close" . dynText $ showClose tz <$> oc

rowTurnPoint
    :: MonadWidget t m
    => Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, ((TaskDistance, TaskDistance, TaskDistance), RawZone))
    -> m ()
rowTurnPoint len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let z = snd . snd <$> iz

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-lat" . dynText $ TP.getLat <$> z
        elClass "td" "td-tp-lng" . dynText $ TP.getLng <$> z
        elClass "td" "td-tp-altitude" . dynText $ TP.getAlt <$> z

rowTurnRadius
    :: MonadWidget t m
    => Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, ((TaskDistance, TaskDistance, TaskDistance), RawZone))
    -> m ()
rowTurnRadius len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let x = snd <$> iz
    let l = (\((b1, _, _), _) -> b1) <$> x
    let z = snd <$> x

    _ <- dyn $ ffor2 i l (\ix leg ->
        case (ix, leg) of
            (1, _) -> return ()
            (_, TaskDistance 0) -> return ()
            (_, leg') ->
                elClass "tr" "tr-tp-distance-leg" $ do
                    el "td" $ text ""
                    elClass "td" "td-tp-distance-leg" . text $ showTaskDistance leg'
                    elAttr "td" ("colspan" =: "5") $ text "")

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-radius" . dynText $ TP.getRadius <$> z
        elClass "td" "td-tp-give" . dynText $ TP.getGiveIn <$> z
        elClass "td" "td-tp-give" . dynText $ TP.getGiveOut <$> z

rowTurnLaunch
    :: MonadWidget t m
    => Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, ((TaskDistance, TaskDistance, TaskDistance), RawZone))
    -> m ()
rowTurnLaunch len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let x = snd <$> iz
    let legSum = (\((_, b2, _), _) -> b2) <$> x
    let z = snd <$> x

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-distance-task" . dynText $ showTaskDistance <$> legSum

rowTurnGoal
    :: MonadWidget t m
    => Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, ((TaskDistance, TaskDistance, TaskDistance), RawZone))
    -> m ()
rowTurnGoal len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let x = snd <$> iz
    let flipSum = (\((_, _, b3), _) -> b3) <$> x
    let z = snd <$> x

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-distance-flip" . dynText $ showTaskDistance <$> flipSum

rowColor :: SpeedSection -> Integer -> T.Text
rowColor Nothing _ = ""
rowColor (Just (ss, es)) ii =
    if | ss == ii -> "start-speed"
       | es == ii -> "end-speed"
       | otherwise -> ""

rowText :: Integer -> SpeedSection -> Integer -> T.Text
rowText len Nothing ii = if len == ii then " *" else ""
rowText len (Just (ss, es)) ii =
    if | ss == ii -> " †"
       | es == ii -> if len == ii then " ‡*" else " ‡"
       | otherwise -> if len == ii then " *" else ""

showStartGate :: TimeZone -> StartGate -> T.Text
showStartGate tz (StartGate t) = showT tz t

showOpen :: TimeZone -> OpenClose -> T.Text
showOpen tz OpenClose{open} = showT tz open

showClose :: TimeZone -> OpenClose -> T.Text
showClose tz OpenClose{close} = showT tz close

module FlareTiming.Task.Turnpoints (tableTask) where

import Debug.Trace
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Comp
    (Task(..), SpeedSection, StartGate(..), UtcOffset(..), OpenClose(..)
    , getAllRawZones, getSpeedSection, getOpenClose, getStartGates
    , getGoalShape
    )
import WireTypes.Route (TaskDistance(..), TaskLegs(..), showTaskDistance)
import WireTypes.Zone (RawZone(..))
import qualified FlareTiming.Turnpoint as TP
import FlareTiming.Time (showT, timeZone)

zero :: TaskDistance
zero = TaskDistance 0

unknownLegs :: [TaskDistance]
unknownLegs = repeat zero

padLegs :: SpeedSection -> [TaskDistance] -> [TaskDistance]
padLegs Nothing xs = xs
padLegs (Just (start, _)) xs =
    prolog <> xs <> unknownLegs
    where
        -- NOTE: The speed section uses 1-based indexing and the legs are
        -- between turnpoints 1-2, 2-3, etc.
        start' = fromIntegral start
        prolog = take (start' - 1) $ repeat zero

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
        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "Turnpoints"
                elClass "div" "content" $ do
                    tableTurnpoints tz x taskLegs
                    return ()

        elClass "div" "tile is-parent is-3" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "Start Gates"
                elClass "div" "content" $ do
                    tableStartGates tz gs
                    return ()

tableTurnpoints
    :: MonadWidget t m
    => TimeZone
    -> Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnpoints tz x taskLegs = do
    let zs = getAllRawZones <$> x
    let goal = getGoalShape <$> x
    let ss = getSpeedSection <$> x
    let oc = (\case [t] -> repeat t; ts -> ts) . getOpenClose <$> x

    pad <- sample . current $ padLegs <$> ss
    len <- sample . current $ fromIntegral . length <$> zs

    let legs' = (maybe unknownLegs (pad . legs)) <$> taskLegs
    let legsSum' = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
    let dd = zipDynWith zip legs' legsSum'
    let ys = ffor3 oc dd zs $ zipWith3 (\a b c -> (a, b, c))

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-distance-task" $ text "Task"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-radius" $ text "Radius"
                    elClass "th" "th-tp-lat" $ text "Latitude"
                    elClass "th" "th-tp-lng" $ text "Longitude"
                    elClass "th" "th-tp-open" $ text "Open"
                    elClass "th" "th-tp-close" $ text "Close"

            _ <- el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (row tz (traceShow len len) ss)

            el "tfoot" $ do
                el "tr" $
                    elAttr "td" ("colspan" =: "8")
                        $ text "† Start of the speed section"
                el "tr" $
                    elAttr "td" ("colspan" =: "8")
                        $ text "‡ End of the speed section"
                el "tr" $
                    elAttr "td" ("colspan" =: "8") . dynText
                        $ (\s -> "* Goal is a " <> s) . T.pack . show <$> goal

    return ()

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

row
    :: MonadWidget t m
    => TimeZone
    -> Integer
    -> Dynamic t SpeedSection
    -> Dynamic t (Integer, (OpenClose, (TaskDistance, TaskDistance), RawZone))
    -> m ()
row tz len ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    rowIntro <- sample . current $ zipDynWith (rowText len) ss i
    let x = snd <$> iz
    let oc = (\(a, _, _) -> a) <$> x
    let l = (\(_, (b1, _), _) -> b1) <$> x
    let s = (\(_, (_, b2), _) -> b2) <$> x
    let z = (\(_, _, c) -> c) <$> x

    _ <- dyn $ ffor2 i l (\ix leg ->
        case (ix, leg) of
            (1, _) -> return ()
            (_, TaskDistance 0) -> return ()
            (_, leg') ->
                el "tr" $ do
                    el "td" $ text ""
                    elClass "td" "td-tp-distance-leg" . text $ showTaskDistance leg'
                    elAttr "td" ("colspan" =: "6") $ text "")

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ (\ix -> (T.pack . show $ ix) <> rowIntro) <$> i
        elClass "td" "td-tp-distance-task" . dynText $ showTaskDistance <$> s
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-radius" . dynText $ TP.getRadius <$> z
        elClass "td" "td-tp-lat" . dynText $ TP.getLat <$> z
        elClass "td" "td-tp-lng" . dynText $ TP.getLng <$> z
        elClass "td" "td-tp-open" . dynText $ showOpen tz <$> oc
        elClass "td" "td-tp-close" . dynText $ showClose tz <$> oc

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

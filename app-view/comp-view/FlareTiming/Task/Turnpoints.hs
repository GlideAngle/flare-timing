module FlareTiming.Task.Turnpoints (tableTask) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Comp
    (Task(..), SpeedSection, StartGate(..), UtcOffset(..)
    , getAllRawZones, getSpeedSection, getStartGates
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
                    tableTurnpoints x taskLegs
                    return ()

        elClass "div" "tile is-parent is-3" $ do
            elClass "article" "tile is-child box" $ do
                elClass "p" "title" $ text "Start Gates"
                elClass "div" "content" $ do
                    tableStartGates tz gs
                    return ()

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnpoints x taskLegs = do
    let zs = getAllRawZones <$> x
    let ss = getSpeedSection <$> x

    pad <- sample . current $ padLegs <$> ss

    let legs' = (maybe unknownLegs (pad . legs)) <$> taskLegs
    let legsSum' = (maybe unknownLegs (pad . legsSum)) <$> taskLegs
    let ys = ffor3 legs' legsSum' zs $ zipWith3 (\a b c -> (a, b, c))

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "5") $ text ""
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-tp-distance")
                        $ text "Distance"
                el "tr" $ do
                    el "th" $ text "#"
                    elClass "th" "th-tp-name" $ text "Name"
                    elClass "th" "th-tp-radius" $ text "Radius"
                    el "th" $ text "Latitude"
                    el "th" $ text "Longitude"
                    elClass "th" "th-tp-distance-leg" $ text "Leg"
                    elClass "th" "th-tp-distance-task" $ text "Task"

            el "tbody" $ do
                simpleList (fmap (zip [1..]) ys) (row ss)

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
                    el "th" $ text "Time"
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
    => Dynamic t SpeedSection
    -> Dynamic t (Integer, (TaskDistance, TaskDistance, RawZone))
    -> m ()
row ss iz = do
    let i = fst <$> iz
    let rowTextColor = zipDynWith rowColor ss i
    let rowIntro = zipDynWith rowText ss i
    let x = snd <$> iz
    let l = (\(a, _, _) -> a) <$> x
    let s = (\(_, b, _) -> b) <$> x
    let z = (\(_, _, c) -> c) <$> x

    _ <- dyn $ ffor2 i l (\ix leg ->
        case (ix, leg) of
            (1, _) -> return ()
            (_, TaskDistance 0) -> return ()
            (_, leg') ->
                el "tr" $ do
                    elAttr "td" ("colspan" =: "5") $ text ""
                    elClass "td" "td-tp-distance" . text $ showTaskDistance leg'
                    el "td" $ text "")

    elDynClass "tr" rowTextColor $ do
        el "td" $ dynText $ T.pack . show <$> i
        elClass "td" "td-tp-name" . dynText $ TP.getName <$> z
        elClass "td" "td-tp-radius" . dynText $ TP.getRadius <$> z
        el "td" . dynText $ TP.getLat <$> z
        el "td" . dynText $ TP.getLng <$> z
        el "td" $ dynText rowIntro
        elClass "td" "td-tp-distance" . dynText $ showTaskDistance <$> s

rowColor :: SpeedSection -> Integer -> T.Text
rowColor Nothing _ = ""
rowColor (Just (ss, es)) ii =
    if | ss == ii -> "has-text-success"
       | es == ii -> "has-text-danger"
       | otherwise -> ""

rowText :: SpeedSection -> Integer -> T.Text
rowText Nothing _ = ""
rowText (Just (ss, es)) ii =
    if | ss == ii -> "Start of Speed Section"
       | es == ii -> "End of Speed Section"
       | otherwise -> ""

showStartGate :: TimeZone -> StartGate -> T.Text
showStartGate tz (StartGate t) = showT tz t

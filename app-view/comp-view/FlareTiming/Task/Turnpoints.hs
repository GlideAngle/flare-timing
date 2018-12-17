module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Comp
    (Task(..), SpeedSection, getSpeedSection, getAllRawZones)
import WireTypes.Route (TaskDistance(..), TaskLegs(..), showTaskDistance)
import WireTypes.Zone (RawZone(..))
import qualified FlareTiming.Turnpoint as TP

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

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnpoints x taskLegs = do
    let ss = getSpeedSection <$> x
    let zs = getAllRawZones <$> x

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

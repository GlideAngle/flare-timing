module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom
import Control.Monad (join)
import qualified Data.Text as T (Text, pack)

import WireTypes.Comp
    (Task(..), SpeedSection, getSpeedSection, getAllRawZones)
import WireTypes.Route (TaskDistance(..), TaskLegs(..), showTaskDistance)
import WireTypes.Zone (RawZone(..))
import qualified FlareTiming.Turnpoint as TP

zero = TaskDistance 0

unknownLegs :: [TaskDistance]
unknownLegs = repeat zero

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLegs)
    -> m ()
tableTurnpoints x taskLegs = do
    let ss = getSpeedSection <$> x
    let zs = getAllRawZones <$> x

    let legs' :: Dynamic _ [TaskDistance]
        legs' = (maybe unknownLegs ((:) zero . legs)) <$> taskLegs

    let legsSum' :: Dynamic _ [TaskDistance]
        legsSum' = (maybe unknownLegs ((:) zero . legsSum)) <$> taskLegs

    let ys :: Dynamic _ [(TaskDistance, TaskDistance, RawZone)]
        ys = ffor3 legs' legsSum' zs $ zipWith3 (\a b c -> (a, b, c))

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "No"
                    el "th" $ text "Leg"
                    el "th" $ text "Distance"
                    el "th" $ text "Id"
                    el "th" $ text "Radius"
                    el "th" $ text "Latitude"
                    el "th" $ text "Longitude"

                simpleList (fmap (zip [1..]) ys) (row ss)
    return ()

row
    :: MonadWidget t m
    => Dynamic t SpeedSection
    -> Dynamic t (Integer, (TaskDistance, TaskDistance, RawZone))
    -> m ()
row ss iz = do
    let color = zipDynWith rowColor ss $ fst <$> iz
    let x = snd <$> iz
    let l = (\(a, _, _) -> a) <$> x
    let s = (\(_, b, _) -> b) <$> x
    let z = (\(_, _, c) -> c) <$> x

    elDynClass "tr" color $ do
        el "td" $ dynText $ T.pack . show . fst <$> iz
        el "td" . dynText $ showTaskDistance <$> l
        el "td" . dynText $ showTaskDistance <$> s
        el "td" . dynText $ TP.getName <$> z
        el "td" . dynText $ TP.getRadius <$> z
        el "td" . dynText $ TP.getLat <$> z
        el "td" . dynText $ TP.getLng <$> z

rowColor :: SpeedSection -> Integer -> T.Text
rowColor Nothing _ = ""
rowColor (Just (ss, es)) ii =
    if | ss == ii -> "has-background-success"
       | es == ii -> "has-background-danger"
       | otherwise -> ""

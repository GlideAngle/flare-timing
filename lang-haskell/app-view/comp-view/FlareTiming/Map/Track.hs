{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Map.Track (tableTrack) where

-- TODO: Find out why hiding Debug.Trace.debugEvent doesn't work.
-- Ambiguous occurrence ‘traceEvent’
-- It could refer to either ‘Debug.Trace.traceEvent’,
--                           imported from ‘Debug.Trace’ at ...
--                           or ‘Reflex.Dom.traceEvent’,
--                           imported from ‘Reflex.Dom’ at ...
--                           (and originally defined in ‘Reflex.Class’)
-- import Debug.Trace hiding (debugEvent)
-- import Reflex.Dom
-- import qualified Debug.Trace as DT
import Prelude hiding (map)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Time.LocalTime (TimeZone)

import FlareTiming.Time (timeZone, showT)
import WireTypes.Cross (TrackFlyingSection(..), TrackScoredSection(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Comp (UtcOffset(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)

tableTrack
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t [(Pilot, ((Pilot, Maybe TrackFlyingSection), (Pilot, Maybe TrackScoredSection)))]
    -> m ()
tableTrack utcOffset xs = do
    tz <- sample . current $ timeZone <$> utcOffset
    let w = ffor xs (pilotIdsWidth . fmap fst)
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("rowspan" =: "2") . dynText $ ffor w hashIdHyphenPilot
                    elAttr "th" ("colspan" =: "3" <> "class" =: "th-map-fixes") $ text "Fixes"
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-map-times") $ text "Times"
                    return ()
                el "tr" $ do
                    el "th" $ text "Flying"
                    el "th" $ text "Scored"
                    el "th" $ text "Unscored"
                    el "th" $ text "Flying"
                    el "th" $ text "Scored"
                    return ()
            el "tbody" $ simpleList xs (row tz w)

    return ()

row
    :: MonadWidget t m
    => TimeZone
    -> Dynamic t Int
    -> Dynamic t (Pilot, ((Pilot, Maybe TrackFlyingSection), (Pilot, Maybe TrackScoredSection)))
    -> m ()
row tz w x = do
    let td = el "td" . dynText
    let p = fst <$> x
    let flying = snd . fst . snd <$> x
    let scored = snd . snd . snd <$> x
    el "tr" $ do
        td $ ffor2 w p showPilot
        td $ ffor flying (maybe "" showFlyingFixes)
        td $ ffor scored (maybe "" showScoredFixes)
        elAttr "td" ("class" =: "td-map-unscored") . dynText $ ffor2 flying scored showUnscored
        td $ ffor flying (maybe "" $ showFlyingTimes tz)
        td $ ffor scored (maybe "" $ showScoredTimes tz)

showFlyingFixes :: TrackFlyingSection -> T.Text
showFlyingFixes TrackFlyingSection{flyingFixes} =
    maybe "" (T.pack . show) flyingFixes

showScoredFixes :: TrackScoredSection -> T.Text
showScoredFixes TrackScoredSection{scoredFixes} =
    maybe "" (T.pack . show) scoredFixes

showUnscored :: Maybe TrackFlyingSection -> Maybe TrackScoredSection -> T.Text
showUnscored (Just TrackFlyingSection{flyingFixes = Just (_, fN)}) (Just TrackScoredSection{scoredFixes = Just (_, sN)}) =
    T.pack . printf "%d" $ fN - sN
showUnscored _ _ = ""

showFlyingTimes :: TimeZone -> TrackFlyingSection -> T.Text
showFlyingTimes tz TrackFlyingSection{flyingTimes} =
    maybe "" (\(t0, tN) -> T.pack $ printf "[%s, %s]" (showT tz t0) (showT tz tN)) flyingTimes

showScoredTimes :: TimeZone -> TrackScoredSection -> T.Text
showScoredTimes tz TrackScoredSection{scoredTimes} =
    maybe "" (\(t0, tN) -> T.pack $ printf "[%s, %s]" (showT tz t0) (showT tz tN)) scoredTimes
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.Event where

import Reflex.Dom
import Reflex.Time (delay)
import Data.Text (Text)
import Control.Monad (when)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))

import WireTypes.Pilot (Pilot(..), nullPilot)
import FlareTiming.Pilot (showPilot)

mkLegend :: MonadWidget t m => Dynamic t Int -> Text -> Pilot -> m ()
mkLegend w classes pp = when (pp /= nullPilot) $ do
    el "tr" $ do
        el "td" $ elClass "span" classes $ text "▩"
        el "td" . dynText $ ffor w (flip showPilot $ pp)
        return ()

legendClasses :: [Text]
legendClasses = fmap ("legend-" <>) ["reach", "effort", "time", "leading", "arrival"]

numLegendPilots :: Int
numLegendPilots = length legendClasses

selectPilots
    :: (MonadIO (Performable m), PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Control.Monad.Fix.MonadFix m)
    => Dynamic t [Pilot]
    -> (Dynamic t [Pilot] -> m (Event t Pilot))
    -> m (Dynamic t Pilot, Event t [Pilot], (Event t Pilot, Event t Pilot, Event t Pilot, Event t Pilot, Event t Pilot))
selectPilots dPilots x = do
    pb <- delay 1 =<< getPostBuild

    ePilot :: Event _ Pilot <- x dPilots
    dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

    let es :: Event _ [Pilot] = updated dPilots
    let eRedraw = leftmost [[] <$ pb, es]

    let nth n = updated
                <$> foldDyn
                        (\ps np ->
                            case take 1 . drop n $ (ps ++ repeat np) of
                                p : _ -> p
                                _ -> np)
                        nullPilot
                        es

    [e1, e2, e3, e4, e5] <- sequence $ nth <$> [0 .. 4]

    -- TODO: Find out why I get "cyclic evaluation in fixIO" if I pass a list
    -- rather than 5-tuple for the legend events with selected pilots.
    return (dPilot, eRedraw, (e1, e2, e3, e4, e5))

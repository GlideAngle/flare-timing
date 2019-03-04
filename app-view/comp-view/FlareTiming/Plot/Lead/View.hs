module FlareTiming.Plot.Lead.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Lead.Plot as P (hgPlot)

import WireTypes.Lead
    (TrackLead(..), LeadingCoefficient(..), LeadingFraction(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilotName)

placings :: [TrackLead] -> [[Double]]
placings = fmap xy

xy :: TrackLead -> [Double]
xy TrackLead{coef = LeadingCoefficient x, frac = LeadingFraction y} =
    [x, y]

lcRange :: [TrackLead] -> (Double, Double)
lcRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackLead{coef = LeadingCoefficient x} -> x) <$> xs

hgPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackLead)]
    -> m ()
hgPlot ld = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $ do
            elClass "div" "tile is-parent" $ do
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ ld'
                                _ <- P.hgPlot (_element_raw elPlot) (lcRange xs) (placings xs)
                                return ())
                            ]

                        ld' <- sample . current $ ld

                    return ()

            elClass "div" "tile is-child" $ tablePilot ld

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Coefficent"
                    el "th" $ text "Fraction"
                    el "th" $ text "Name"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowLead . splitDynPure)

    return ()

rowLead
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m ()
rowLead p av = do
    el "tr" $ do
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ showFrac . frac <$> av
        el "td" . dynText $ showPilotName <$> p

        return ()

showCoef :: LeadingCoefficient -> T.Text
showCoef (LeadingCoefficient lc) = T.pack $ printf "%.3f" lc

showFrac :: LeadingFraction -> T.Text
showFrac (LeadingFraction x) = T.pack $ printf "%.3f" x


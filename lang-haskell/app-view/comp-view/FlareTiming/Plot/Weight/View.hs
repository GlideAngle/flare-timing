module FlareTiming.Plot.Weight.View (hgWeightPlot, pgWeightPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Weight.Plot as P (hgWeightPlot, pgWeightPlot)

import WireTypes.Comp (Tweak(..))
import WireTypes.Point
    ( GoalRatio(..)
    , Allocation(..)
    , Weights(..)
    , DistanceWeight(..)
    , ArrivalWeight(..)
    , LeadingWeight(..)
    , TimeWeight(..)
    , zeroWeights
    )

textf :: String -> Double -> T.Text
textf fmt d = T.pack $ printf fmt d

hgWeightPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
hgWeightPlot tweak' alloc' = do
    tweak <- sample . current $ tweak'
    alloc <- sample . current $ alloc'
    let gr@(GoalRatio gr') = maybe (GoalRatio 0) goalRatio alloc
    let w = maybe zeroWeights weight alloc
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-weight") <> ("style" =: "height: 640px;width: 640px")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                _ <- P.hgWeightPlot (_element_raw elPlot) tweak gr w
                return ())
            ]

    let Weights
            { distance = DistanceWeight d
            , arrival = ArrivalWeight a
            , leading = LeadingWeight l
            , time = TimeWeight t
            } = w

    elClass "div" "level" $ do
        elClass "div" "level-item" $
            el "ul" $
                elClass "li" "legend-goal-ratio" . text $
                    textf "%.3f arrival ratio ⇒" gr'
        elClass "div" "level-item" $
            el "ul" $ do
                elClass "li" "legend-reach" . text $
                    textf "▩ %.3f weight on distance" d
                elClass "li" "legend-time" . text $
                    textf "▩ %.3f weight on time" t
                elClass "li" "legend-leading" . text $
                    textf "▩ %.3f weight on leading" l
                elClass "li" "legend-arrival" . text $
                    textf "▩ %.3f weight on arrival" a

    return ()

pgWeightPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
pgWeightPlot tweak' alloc' = do
    tweak <- sample . current $ tweak'
    alloc <- sample . current $ alloc'
    let w = maybe zeroWeights weight alloc
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "pg-plot-weight") <> ("style" =: "height: 640px;width: 640px")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                _ <- P.pgWeightPlot (_element_raw elPlot) tweak gr w
                return ())
            ]

    let Weights
            { distance = DistanceWeight d
            , leading = LeadingWeight l
            , time = TimeWeight t
            } = w

    el "ul" $ do
        elClass "li" "legend-reach" . text $
            textf "▩ %.3f weight on distance" d
        elClass "li" "legend-time" . text $
            textf "▩ %.3f weight on time" t
        elClass "li" "legend-leading" . text $
            textf "▩ %.3f weight on leading" l

    return ()

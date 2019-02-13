module FlareTiming.Plot.View (viewPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.FunctionPlot as P (hgPlot, pgPlot)

import WireTypes.Comp (Discipline(..))
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

viewPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Allocation)
    -> m ()
viewPlot hgOrPg alloc = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-4" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $
                    elClass "article" "tile is-child box" $ do
                        _ <- dyn $ ffor hgOrPg (\case
                                HangGliding -> hgPlot alloc
                                Paragliding -> pgPlot alloc)
                        return ()

textf :: String -> Double -> T.Text
textf fmt d = T.pack $ printf fmt d

hgPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
hgPlot alloc' = do
    alloc <- sample . current $ alloc'
    let w = maybe zeroWeights weight alloc
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                _ <- P.hgPlot (_element_raw elPlot) gr w
                return ())
            ]

    let Weights
            { distance = DistanceWeight d
            , arrival = ArrivalWeight a
            , leading = LeadingWeight l
            , time = TimeWeight t
            } = w

    el "ul" $ do
        elClass "li" "legend-distance" . text $
            textf "○ %.3f weight on distance" d
        elClass "li" "legend-time" . text $
            textf "○ %.3f weight on time" t
        elClass "li" "legend-leading" . text $
            textf "○ %.3f weight on leading" l
        elClass "li" "legend-arrival" . text $
            textf "○ %.3f weight on arrival" a

    return ()

pgPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
pgPlot alloc' = do
    alloc <- sample . current $ alloc'
    let w = maybe zeroWeights weight alloc
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "pg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                _ <- P.pgPlot (_element_raw elPlot) gr w
                return ())
            ]

    let Weights
            { distance = DistanceWeight d
            , leading = LeadingWeight l
            , time = TimeWeight t
            } = w

    el "ul" $ do
        elClass "li" "legend-distance" . text $
            textf "○ %.3f weight on distance" d
        elClass "li" "legend-time" . text $
            textf "○ %.3f weight on time" t
        elClass "li" "legend-leading" . text $
            textf "○ %.3f weight on leading" l

    return ()

module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.FunctionPlot as P (hgPlot, pgPlot)

import WireTypes.Comp (Discipline(..))
import WireTypes.Point (Allocation(..), GoalRatio(..), zeroWeights)

viewPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Allocation)
    -> m ()
viewPlot hgOrPg alloc = do
    _ <- dyn $ ffor hgOrPg (\case
            HangGliding -> hgPlot alloc
            Paragliding -> pgPlot alloc)
    return ()

hgPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
hgPlot alloc' = do
    alloc <- sample . current $ alloc'
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                let w = maybe zeroWeights weight alloc
                _ <- P.hgPlot (_element_raw elPlot) gr w
                return ())
            ]

    return ()

pgPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
pgPlot alloc' = do
    alloc <- sample . current $ alloc'
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "pg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                let w = maybe zeroWeights weight alloc
                _ <- P.pgPlot (_element_raw elPlot) gr w
                return ())
            ]

    return ()

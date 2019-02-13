module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.FunctionPlot as P (hgPlot, pgPlot)

import WireTypes.Point (Allocation(..), GoalRatio(..))

viewPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
viewPlot alloc' = do
    alloc <- sample . current $ alloc'
    pb <- delay 1 =<< getPostBuild
    (hgPlot, _) <- elAttr' "div" (("id" =: "hg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    (pgPlot, _) <- elAttr' "div" (("id" =: "pg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                let gr = maybe (GoalRatio 0) goalRatio alloc
                _ <- P.hgPlot (_element_raw hgPlot) gr
                _ <- P.pgPlot (_element_raw pgPlot) gr
                return ())
            ]

    return ()

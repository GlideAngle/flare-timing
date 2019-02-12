module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)

import qualified FlareTiming.Plot.FunctionPlot as P (plot)

viewPlot :: MonadWidget t m => m ()
viewPlot = plot

plot :: MonadWidget t m => m ()
plot = do
    pb <- delay 1 =<< getPostBuild
    (ePlot, _) <- elAttr' "div" (("id" =: "plot") <> ("style" =: "height: 680px;width: 100%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                _ <- P.plot (_element_raw ePlot)
                return ())
            ]

    return ()

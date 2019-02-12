module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)

import qualified FlareTiming.Plot.FunctionPlot as P (hgPlot, pgPlot)

viewPlot :: MonadWidget t m => m ()
viewPlot = plot

plot :: MonadWidget t m => m ()
plot = do
    pb <- delay 1 =<< getPostBuild
    (hgPlot, _) <- elAttr' "div" (("id" =: "hg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    (pgPlot, _) <- elAttr' "div" (("id" =: "pg-plot") <> ("style" =: "height: 400px;width: 50%")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                _ <- P.hgPlot (_element_raw hgPlot)
                _ <- P.pgPlot (_element_raw pgPlot)
                return ())
            ]

    return ()

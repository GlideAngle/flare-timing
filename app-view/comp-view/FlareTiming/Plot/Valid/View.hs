module FlareTiming.Plot.Valid.View (hgPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Valid.Plot as P (hgPlot)

hgPlot
    :: MonadWidget t m
    => m ()
hgPlot = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-launch") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.hgPlot (_element_raw elPlot)
                                return ())
                            ]

                    return ()

    return ()

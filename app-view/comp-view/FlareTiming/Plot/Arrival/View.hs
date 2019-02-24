module FlareTiming.Plot.Arrival.View (hgPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Arrival.Plot as P (hgPlot)

import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (GoalRatio(..), Allocation(..))

hgPlot
    :: MonadWidget t m
    => Maybe PilotsFlying
    -> Maybe Allocation
    -> m ()
hgPlot Nothing _ = return ()
hgPlot (Just pf) alloc = do
    let gr = maybe (GoalRatio 0) goalRatio alloc
    pb <- delay 1 =<< getPostBuild
    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival") <> ("style" =: "height: 360px;width: 360px")) $ return ()
    rec performEvent_ $ leftmost
            [ ffor pb (\_ -> liftIO $ do
                _ <- P.hgPlot (_element_raw elPlot) pf gr
                return ())
            ]

    return ()

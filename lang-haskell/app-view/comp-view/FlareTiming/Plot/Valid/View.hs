module FlareTiming.Plot.Valid.View
    ( launchValidityPlot
    , timeValidityPlot
    , reachValidityPlot
    , stopByReachValidityPlot
    , stopByLandedValidityPlot
    , stopByVaryValidityPlot
    ) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import WireTypes.Validity (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import qualified FlareTiming.Plot.Valid.Plot as P
    ( launchPlot, timePlot, reachPlot
    , stopByReachPlot, stopByLandedPlot, stopByVaryPlot
    )

launchValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
launchValidityPlot Validity{launch = vy} ValidityWorking{launch = vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-launch") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.launchPlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

timeValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
timeValidityPlot Validity{time = vy} ValidityWorking{time = vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-time") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.timePlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

reachValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
reachValidityPlot Validity{distance = vy} ValidityWorking{distance = vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-reach") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.reachPlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

stopByReachValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
stopByReachValidityPlot Validity{stop = Nothing} _ = return ()
stopByReachValidityPlot _ ValidityWorking{stop = Nothing} = return ()
stopByReachValidityPlot Validity{stop = Just vy} ValidityWorking{stop = Just vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-stop-by-reach") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.stopByReachPlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

stopByLandedValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
stopByLandedValidityPlot Validity{stop = Nothing} _ = return ()
stopByLandedValidityPlot _ ValidityWorking{stop = Nothing} = return ()
stopByLandedValidityPlot Validity{stop = Just vy} ValidityWorking{stop = Just vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-stop-by-landed") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.stopByLandedPlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

stopByVaryValidityPlot
    :: MonadWidget t m
    => Validity
    -> ValidityWorking
    -> m ()
stopByVaryValidityPlot Validity{stop = Nothing} _ = return ()
stopByVaryValidityPlot _ ValidityWorking{stop = Nothing} = return ()
stopByVaryValidityPlot Validity{stop = Just vy} ValidityWorking{stop = Just vw} = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-valid-stop-by-vary") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                _ <- P.stopByVaryPlot (_element_raw elPlot) vy vw
                                return ())
                            ]

                    return ()

    return ()

module FlareTiming.Plot.Valid (validPlot) where

import Reflex.Dom

import WireTypes.Validity (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import FlareTiming.Plot.Valid.View
    ( launchValidityPlot
    , timeValidityPlot
    , reachValidityPlot
    , stopByReachValidityPlot
    , stopByLandedValidityPlot
    , stopByVaryValidityPlot
    )

validPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> m ()
validPlot vy vw = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> launchValidityPlot vy'' vw''
                        _ -> return ())

                return ()

            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> reachValidityPlot vy'' vw''
                        _ -> return ())

                return ()

            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> timeValidityPlot vy'' vw''
                        _ -> return ())

                return ()

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-parent" $ do
            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> stopByReachValidityPlot vy'' vw''
                        _ -> return ())

                return ()

            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> stopByLandedValidityPlot vy'' vw''
                        _ -> return ())

                return ()

            elClass "article" "tile is-4 is-child" $ do
                _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                    case (vy', vw') of
                        (Just vy'', Just vw'') -> stopByVaryValidityPlot vy'' vw''
                        _ -> return ())

                return ()

    return ()

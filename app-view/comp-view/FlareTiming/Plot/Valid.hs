module FlareTiming.Plot.Valid (validPlot) where

import Reflex.Dom

import WireTypes.Validity (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import FlareTiming.Plot.Valid.View (launchValidityPlot, timeValidityPlot)

validPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> m ()
validPlot vy vw = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-4 is-parent" $ do
                elClass "article" "tile is-child" $ do
                    _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                        case (vy', vw') of
                            (Just vy'', Just vw'') -> launchValidityPlot vy'' vw''
                            _ -> return ())

                    return ()

                elClass "article" "tile is-child" $ do
                    _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                        case (vy', vw') of
                            (Just vy'', Just vw'') -> timeValidityPlot vy'' vw''
                            _ -> return ())

                    return ()

    return ()

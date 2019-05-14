module FlareTiming.Plot.Valid (validPlot) where

import Reflex.Dom

import WireTypes.Validity (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import FlareTiming.Plot.Valid.View (hgPlot)

validPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> m ()
validPlot vy vw = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-4 is-parent" $
                elClass "article" "tile is-child" $ do
                    _ <- dyn $ ffor2 vy vw (\vy' vw' -> do
                        case (vy', vw') of
                            (Just vy'', Just vw'') -> hgPlot vy'' vw''
                            _ -> return ())

                    return ()

    return ()

{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.LeadArea.Plot (leadAreaPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-lead'\
    \, title: 'Area'\
    \, width: 480\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Distance in Speed Section [km]', domain: [$2, $3]}\
    \, yAxis: {label: 'Time in Speed Section [s]', domain: [$4, $5]}\
    \, data: [{\
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , graphType: 'polyline'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

leadAreaPlot
    :: IsElement e
    => e
    -> ((Double, Double), (Double, Double))
    -> [[Double]]
    -> IO Plot
leadAreaPlot e ((xMin, xMax), (yMin, yMax)) xs = do
    let xPad = (\case 0 -> 1.0; x -> x) . abs $ (xMax - xMin) / 40
    xMin' <- toJSVal $ xMin - xPad
    xMax' <- toJSVal $ xMax + xPad

    let yPad = (\case 0 -> 1.0; x -> x) . abs $ (yMax - yMin) / 40
    yMin' <- toJSVal $ yMin - yPad
    yMax' <- toJSVal $ yMax + yPad

    xs' <- toJSValListOf $ nub xs

    Plot <$> plot_ (unElement . toElement $ e) xMin' xMax' yMin' yMax' xs'

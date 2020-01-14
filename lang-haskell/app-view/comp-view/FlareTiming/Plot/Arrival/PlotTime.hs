{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Arrival.PlotTime (hgPlotTime) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-arrival-time'\
    \, title: 'Arrival Time Point Distribution'\
    \, width: 640\
    \, height: 460\
    \, disableZoom: true\
    \, xAxis: {label: 'Arrival Lag (hours)', domain: [0, $2]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $3\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , range: [0, $2]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 3 }\
    \  , range: [0, $2]\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plotTime_ :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlotTime
    :: IsElement e
    => e
    -> Double
    -> [[Double]]
    -> IO Plot
hgPlotTime e lagMax xys = do
    let xMax = lagMax

    let xyFns :: [[Double]] =
            [ [x', fnTime x']
            | x <- [0.0, 1.0 .. 10.0 * xMax]
            , let x' = 0.1 * x
            ]

    xMax' <- toJSVal xMax
    xys' <- toJSValListOf xys
    xyFns' <- toJSValListOf xyFns

    Plot <$> plotTime_ (unElement . toElement $ e) xMax' xyFns' xys'

fnTime :: Double -> Double
fnTime lag = max 0 $ (1 - (2.0/3.0) * lag) ** 3

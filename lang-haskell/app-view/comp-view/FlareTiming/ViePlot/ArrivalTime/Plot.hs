{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.ViePlot.ArrivalTime.Plot (hgViePlotTime) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (ToJSVal(..), Element(..), toElement, toJSVal, toJSValListOf)
import FlareTiming.Plot.Foreign (Plot(..))
import FlareTiming.Plot.Event (uncurry5, unpackSelect)

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-arrival-time'\
    \, title: 'Arrival Time Point Distribution'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Arrival Lag (hours)', domain: [0 - 0.05 * $2, 1.05 * $2]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $3\
    \  , fnType: 'points'\
    \  , color: '#808080'\
    \  , range: [0, $2]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#1e1e1e'\
    \  , attr: { r: 3 }\
    \  , range: [0, $2]\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $7\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $8\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $9\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plotTime_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgViePlotTime
    :: IsElement e
    => e
    -> Double
    -> [[Double]] -- ^ All
    -> [[Double]] -- ^ Selected
    -> IO Plot
hgViePlotTime e lagMax xs ys = do
    let xMax = lagMax

    let xyFns :: [[Double]] =
            [ [x', fnTime x']
            | x <- [0.0, 1.0 .. 11.0 * xMax]
            , let x' = 0.1 * x
            ]

    xMax' <- toJSVal xMax
    xyFns' <- toJSValListOf xyFns
    xs' <- toJSValListOf xs
    ys' <- unpackSelect ys

    Plot <$> (uncurry5 $ plotTime_ (unElement . toElement $ e) xMax' xyFns' xs') ys'

fnTime :: Double -> Double
fnTime lag = max 0 $ (1 - (2.0/3.0) * lag) ** 3

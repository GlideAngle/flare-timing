{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Arrival.Plot (hgPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-arrival'\
    \, title: 'Arrival Point Distribution'\
    \, width: 640\
    \, height: 460\
    \, disableZoom: true\
    \, xAxis: {label: 'Arrival Placing', domain: [0, $2 + 1]}\
    \, yAxis: {domain: [0, 1.01]}\
    \, data: [{\
    \    points: $3\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , range: [1, $2]\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 2 }\
    \  , range: [1, $2]\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 4 }\
    \  , range: [1, $2]\
    \  , graphType: 'scatter'\
    \  }]\
    \, annotations: [{\
    \    y: 0.2\
    \  , text: 'minimum possible fraction'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot
    :: IsElement e
    => e
    -> [[Double]]
    -> [[Double]]
    -> IO Plot
hgPlot e xs ys = do
    let n :: Integer = fromIntegral $ length xs + length ys

    n' <- toJSVal (fromIntegral n :: Double)
    let xy :: [[Double]] =
            [ [x', fn n x']
            | x <- [1 .. 10 * n]
            , let x' = 0.1 * fromIntegral x
            ]

    xy' <- toJSValListOf xy
    xs' <- toJSValListOf xs
    ys' <- toJSValListOf $ nub ys

    Plot <$> plot_ (unElement . toElement $ e) n' xy' xs' ys'

fn :: Integer -> Double -> Double
fn n x = 0.2 + 0.037 * y + 0.13 * y**2 + 0.633 * y**3
    where
        y :: Double
        y = 1.0 - (x - 1.0) / (fromIntegral n)

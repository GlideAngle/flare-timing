{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Valid.Plot
    ( Plot(..)
    , hgPlot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSValListOf)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-launch'\
    \, title: 'Launch Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: 'Launch Validity Ratio', domain: [0, 1]}\
    \, yAxis: {domain: [0, 1.01]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#000000'\
    \  , range: [0, 1]\
    \  , graphType: 'polyline'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> IO JSVal

hgPlot :: IsElement e => e -> IO Plot
hgPlot e = do
    let xy :: [[Double]] =
            [ [x', fn x']
            | x <- [0 :: Integer .. 100]
            , let x' = 0.01 * fromIntegral x
            ]

    xy' <- toJSValListOf xy

    Plot <$> hgPlot_ (unElement . toElement $ e) xy'

fn :: Double -> Double
fn x = 0.027 * x + 2.917 * x**2 - 1.944 * x**3

{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Time.Plot
    ( Plot(..)
    , hgPlot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-speed'\
    \, title: 'Time Point Distribution'\
    \, width: 720\
    \, height: 460\
    \, disableZoom: true\
    \, xAxis: {label: 'Time (Hours)', domain: [$2, $3]}\
    \, yAxis: {domain: [-0.1, 1.01]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 2 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]]
    -> IO Plot
hgPlot e (tMin, tMax) xs = do
    let xy :: [[Double]] =
            [ [x', fn tMin x']
            | x <- [0 :: Integer .. 199]
            , let step = (tMax - tMin) / 200
            , let x' = tMin + step * fromIntegral x
            ]

    let pad = (tMax - tMin) / 40
    tMin' <- toJSVal $ tMin - pad
    tMax' <- toJSVal $ tMax + pad

    xy' <- toJSValListOf xy
    xs' <- toJSValListOf $ nub xs

    Plot <$> hgPlot_ (unElement . toElement $ e) tMin' tMax' xy' xs'

fn :: Double -> Double -> Double
fn tMin x =
    max 0.0 $ 1.0 - ((x - tMin)**2/tMin**(1.0/2.0))**(1.0/3.0)

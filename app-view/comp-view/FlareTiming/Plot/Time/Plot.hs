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
    \{ target: '#hg-plot-lead'\
    \, title: 'Time Point Distribution'\
    \, width: 720\
    \, height: 460\
    \, disableZoom: true\
    \, xAxis: {label: 'Time', domain: [$2 - 1, $3 + 1]}\
    \, yAxis: {domain: [-0.1, 1.01]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: 'blue'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: 'blue'\
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
hgPlot e (lcMin, lcMax) xs = do
    let delta = (lcMax - lcMin) / 200

    let xy :: [[Double]] =
            [ [x', fn lcMin x']
            | x <- [0 :: Integer .. 199]
            , let x' = lcMin + delta * fromIntegral x
            ]

    lcMin' <- toJSVal lcMin
    lcMax' <- toJSVal lcMax
    xy' <- toJSValListOf xy
    xs' <- toJSValListOf $ nub xs

    Plot <$> hgPlot_ (unElement . toElement $ e) lcMin' lcMax' xy' xs'

fn :: Double -> Double -> Double
fn lcMin x =
    max 0.0 $ 1.0 - ((x - lcMin)**2/lcMin**(1.0/2.0))**(1.0/3.0)

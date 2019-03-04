{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Effort.Plot
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
    \{ target: '#hg-plot-effort'\
    \, title: 'Effort Point Distribution'\
    \, width: 720\
    \, height: 460\
    \, disableZoom: true\
    \, xAxis: {label: 'Landout (km)', domain: [$2, $3]}\
    \, yAxis: {domain: [-0.1, 1.01]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
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
hgPlot e (dMin, dMax) xs = do
    -- NOTE: Some pilots landout heading away from goal at the start and will
    -- have made negative distance along the course. Don't draw the line in the
    -- negative xy quadrant.
    let dMin' = max 0 dMin

    let xy :: [[Double]] =
            [ [x', fn dMax x']
            | x <- [0 :: Integer .. 199]
            , let step = abs $ (dMax - dMin') / 200
            , let x' = dMin' + step * fromIntegral x
            ]

    let pad = (\case 0 -> 1; x -> x) . abs $ (dMax - dMin') / 40
    dMin'' <- toJSVal $ dMin' - pad
    dMax' <- toJSVal $ dMax + pad

    xy' <- toJSValListOf xy
    xs' <- toJSValListOf $ nub xs

    Plot <$> hgPlot_ (unElement . toElement $ e) dMin'' dMax' xy' xs'

fn :: Double -> Double -> Double
fn 0 _ = 0
fn dMax x = x / dMax

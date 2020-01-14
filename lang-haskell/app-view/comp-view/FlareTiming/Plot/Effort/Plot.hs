{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Effort.Plot (effortPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-effort'\
    \, title: 'Effort Point Distribution'\
    \, width: 640\
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
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

effortPlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]]
    -> IO Plot
effortPlot e (dMin, dMax) xs = do
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

    Plot <$> plot_ (unElement . toElement $ e) dMin'' dMax' xy' xs'

fn :: Double -> Double -> Double
fn 0 _ = 0
fn dMax x = x / dMax

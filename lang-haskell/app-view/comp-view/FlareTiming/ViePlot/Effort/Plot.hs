{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.ViePlot.Effort.Plot (effortViePlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))
import FlareTiming.Plot.Event (uncurry5, unpackSelect)

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-effort'\
    \, title: 'Effort Point Distribution'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Landout (km)', domain: [$2, $3]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#808080'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#1e1e1e'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $7\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $8\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $9\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $10\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

effortViePlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]] -- ^ All xy pairs
    -> [[Double]] -- ^ Selected xy pairs
    -> IO Plot
effortViePlot e (dMin, dMax) xs ys = do
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
    ys' <- unpackSelect ys

    Plot <$> (uncurry5 $ plot_ (unElement . toElement $ e) dMin'' dMax' xy' xs') ys'

fn :: Double -> Double -> Double
fn 0 _ = 0
fn dMax x = x / dMax

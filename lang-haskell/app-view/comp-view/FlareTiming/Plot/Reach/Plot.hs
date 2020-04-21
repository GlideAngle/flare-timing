{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Reach.Plot (reachPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-reach'\
    \, title: 'Reach Point Distribution'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Flown Reach (km)', domain: [$2, $3]}\
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
    \  , color: '#1e1e1e'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $7\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $8\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $9\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $10\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $11\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $12\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $13\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $14\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $15\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $16\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 9 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

reachPlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]] -- ^ All xy pairs for reach
    -> [[Double]] -- ^ All xy pairs for bonus reach
    -> [[Double]] -- ^ Selected xy pairs for reach
    -> [[Double]] -- ^ Selected xy pairs for bonus reach
    -> IO Plot
reachPlot e (dMin, dMax) xs xsBonus ys ysBonus = do
    let xy :: [[Double]] =
            [ [x', fn dMax x']
            | x <- [0 :: Integer .. 199]
            , let step = abs $ (dMax - dMin) / 200
            , let x' = dMin + step * fromIntegral x
            ]

    let pad = (\case 0 -> 1; x -> x) . abs $ (dMax - dMin) / 40
    dMin' <- toJSVal $ dMin - pad
    dMax' <- toJSVal $ dMax + pad

    xy' <- toJSValListOf xy
    xs' <- toJSValListOf $ nub xs
    xsBonus' <- toJSValListOf $ nub xsBonus

    ys1 <- toJSValListOf $ take 1 ys
    ys2 <- toJSValListOf $ take 1 $ drop 1 ys
    ys3 <- toJSValListOf $ take 1 $ drop 2 ys
    ys4 <- toJSValListOf $ take 1 $ drop 3 ys
    ys5 <- toJSValListOf $ take 1 $ drop 4 ys

    ysB1 <- toJSValListOf $ take 1 ysBonus
    ysB2 <- toJSValListOf $ take 1 $ drop 1 ysBonus
    ysB3 <- toJSValListOf $ take 1 $ drop 2 ysBonus
    ysB4 <- toJSValListOf $ take 1 $ drop 3 ysBonus
    ysB5 <- toJSValListOf $ take 1 $ drop 4 ysBonus

    Plot <$>
        plot_
            (unElement . toElement $ e)
            dMin' dMax' xy'
            xs' xsBonus'
            ys1 ys2 ys3 ys4 ys5
            ysB1 ysB2 ysB3 ysB4 ysB5

fn :: Double -> Double -> Double
fn 0 _ = 0
fn dMax x = x / dMax

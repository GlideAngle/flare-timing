{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.LeadCoef.Plot (leadCoefPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-lead'\
    \, title: 'Lead Point Distribution'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Leading Coefficient', domain: [$2, $3]}\
    \, yAxis: {domain: [-0.1, 1.01]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

leadCoefPlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]]
    -> IO Plot
leadCoefPlot e (lcMin, lcMax) xs = do
    let xyfn :: [[Double]] =
            [ [x', fnGAP lcMin x']
            | x <- [0 :: Integer .. 199]
            , let step = abs $ (lcMax - lcMin) / 200
            , let x' = lcMin + step * fromIntegral x
            ]

    let xyfnFS :: [[Double]] =
            [ [x', fnFS lcMin x']
            | x <- [0 :: Integer .. 199]
            , let step = abs $ (lcMax - lcMin) / 200
            , let x' = lcMin + step * fromIntegral x
            ]
    let pad = (\case 0 -> 1.0; x -> x) . abs $ (lcMax - lcMin) / 40
    lcMin' <- toJSVal $ lcMin - pad
    lcMax' <- toJSVal $ lcMax + pad
    xyfn' <- toJSValListOf xyfn
    xyfnFS' <- toJSValListOf xyfnFS
    xs' <- toJSValListOf $ nub xs

    Plot <$> plot_ (unElement . toElement $ e) lcMin' lcMax' xyfn' xs' xyfnFS'

-- | The equation from the GAP rules.
fnGAP :: Double -> Double -> Double
fnGAP lcMin lc =
    max 0.0 $ 1.0 - ((lc - lcMin)**2/lcMin**(1.0/2.0))**(1.0/3.0)

-- | The equation from the FS implementation.
fnFS :: Double -> Double -> Double
fnFS lcMin lc =
    max 0.0 $ 1.0 - ((lc - lcMin)/lcMin**(1.0/2.0))**(2.0/3.0)

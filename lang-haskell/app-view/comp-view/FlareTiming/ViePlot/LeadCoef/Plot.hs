{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.ViePlot.LeadCoef.Plot (leadCoefViePlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))
import FlareTiming.Plot.Event (uncurry5, unpackSelect)

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-lead'\
    \, title: 'Lead Point Distribution'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Leading Coefficient', domain: [$2, $3]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: '#808080'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#808080'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
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
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

leadCoefViePlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]] -- ^ All xy pairs
    -> [[Double]] -- ^ Selected xy pairs
    -> IO Plot
leadCoefViePlot e (lcMin, lcMax) xs ys = do
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
    ys' <- unpackSelect ys

    Plot <$> (uncurry5 $ plot_ (unElement . toElement $ e) lcMin' lcMax' xyfn' xyfnFS' xs') ys'

-- | The equation from the GAP rules.
fnGAP :: Double -> Double -> Double
fnGAP lcMin lc =
    max 0.0 $ 1.0 - ((lc - lcMin)**2/lcMin**(1.0/2.0))**(1.0/3.0)

-- | The equation from the FS implementation.
fnFS :: Double -> Double -> Double
fnFS lcMin lc =
    max 0.0 $ 1.0 - ((lc - lcMin)/lcMin**(1.0/2.0))**(2.0/3.0)

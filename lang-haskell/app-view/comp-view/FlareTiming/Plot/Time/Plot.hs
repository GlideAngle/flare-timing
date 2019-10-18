{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Time.Plot (timePlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-speed'\
    \, title: 'Time Point Distribution'\
    \, width: 640\
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
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $5\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 2 }\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

timePlot
    :: IsElement e
    => e
    -> (Double, Double)
    -> [[Double]]
    -> IO Plot
timePlot e (tMin, tMax) xs = do
    let xyfn :: [[Double]] =
            [ [x', fnGAP tMin x']
            | x <- [0 .. 199 :: Integer]
            , let step = abs $ (tMax - tMin) / 200
            , let x' = tMin + step * fromIntegral x
            ]

    let xyfnFS :: [[Double]] =
            [ [x', fnFS tMin x']
            | x <- [0 .. 199 :: Integer]
            , let step = abs $ (tMax - tMin) / 200
            , let x' = tMin + step * fromIntegral x
            ]

    -- NOTE: 0.083 hr = 5 min
    let pad = (\case 0 -> 0.083; x -> x) . abs $ (tMax - tMin) / 40
    tMin' <- toJSVal $ tMin - pad
    tMax' <- toJSVal $ tMax + pad

    xyfn' <- toJSValListOf xyfn
    xyfnFS' <- toJSValListOf xyfnFS
    xs' <- toJSValListOf $ nub xs

    Plot <$> plot_ (unElement . toElement $ e) tMin' tMax' xyfn' xs' xyfnFS'

-- | The equation from the GAP rules.
fnGAP :: Double -> Double -> Double
fnGAP tMin t =
    max 0.0 $ 1.0 - ((t - tMin)**2/tMin**(1.0/2.0))**(1.0/3.0)

-- | The equation from the FS implementation.
fnFS :: Double -> Double -> Double
fnFS tMin t =
    max 0.0 $ 1.0 - ((t - tMin)/tMin**(1.0/2.0))**(2.0/3.0)

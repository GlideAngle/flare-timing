{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.ViePlot.LeadArea.Plot (leadAreaViePlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)
import Data.List (nub)
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-lead'\
    \, title: 'Area'\
    \, width: 700\
    \, height: 640\
    \, disableZoom: true\
    \, xAxis: {label: 'Distance in Speed Section [km]', domain: [$2, $3]}\
    \, yAxis: {label: 'Time in Speed Section [s]', domain: [$4, $5]}\
    \, data: [{\
    \    points: $6\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $7\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $8\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $9\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $10\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $11\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $12\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $13\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $14\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $15\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $16\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $17\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $18\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $19\
    \  , fnType: 'points'\
    \  , color: '#e41a1c'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $20\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  }]\
    \})"
    plot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

leadAreaViePlot
    :: IsElement e
    => e
    -> ((Double, Double), (Double, Double))
    -> [[[Double]]]
    -> [[[Double]]]
    -> [[[Double]]]
    -> IO Plot

leadAreaViePlot
    e
    ((xMin, xMax), (yMin, yMax))
    (xs0 : xs1 : xs2 : xs3 : xs4 : _)
    (ys0 : ys1 : ys2 : ys3 : ys4 : _)
    (zs0 : zs1 : zs2 : zs3 : zs4 : _) = do
    let xPad = (\case 0 -> 1.0; x -> x) . abs $ (xMax - xMin) / 40
    xMin' <- toJSVal $ xMin - xPad
    xMax' <- toJSVal $ xMax + xPad

    let yPad = (\case 0 -> 1.0; x -> x) . abs $ (yMax - yMin) / 40
    yMin' <- toJSVal $ yMin - yPad
    yMax' <- toJSVal $ yMax + yPad

    xs0' <- toJSValListOf $ nub xs0
    xs1' <- toJSValListOf $ nub xs1
    xs2' <- toJSValListOf $ nub xs2
    xs3' <- toJSValListOf $ nub xs3
    xs4' <- toJSValListOf $ nub xs4

    ys0' <- toJSValListOf $ nub ys0
    ys1' <- toJSValListOf $ nub ys1
    ys2' <- toJSValListOf $ nub ys2
    ys3' <- toJSValListOf $ nub ys3
    ys4' <- toJSValListOf $ nub ys4

    zs0' <- toJSValListOf $ nub zs0
    zs1' <- toJSValListOf $ nub zs1
    zs2' <- toJSValListOf $ nub zs2
    zs3' <- toJSValListOf $ nub zs3
    zs4' <- toJSValListOf $ nub zs4

    Plot <$> plot_ (unElement . toElement $ e) xMin' xMax' yMin' yMax' xs0' xs1' xs2' xs3' xs4' ys0' ys1' ys2' ys3' ys4' zs0' zs1' zs2' zs3' zs4'

leadAreaPlot e r _ _ _ = let zs = take 4 (repeat []) in leadAreaPlot e r zs zs zs

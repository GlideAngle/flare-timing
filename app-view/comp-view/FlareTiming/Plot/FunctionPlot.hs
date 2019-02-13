{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.FunctionPlot
    ( Plot(..)
    , hgPlot
    , pgPlot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal)

import WireTypes.Point
    ( GoalRatio(..)
    , DistanceWeight(..)
    , ArrivalWeight(..)
    , LeadingWeight(..)
    , TimeWeight(..)
    , Weights(..)
    )

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot'\
    \, title: 'Split of Available Points'\
    \, width: 360\
    \, height: 400\
    \, disableZoom: true\
    \, xAxis: {label: 'Fraction of Pilots in Goal', domain: [0, 1]}\
    \, yAxis: {domain: [0, 1]}\
    \, data: [{\
    \    fn: '0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3'\
    \  , nSamples: 101\
    \  , color: 'blue'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $3]]\
    \  , fnType: 'points'\
    \  , color: 'blue'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4'\
    \  , nSamples: 101\
    \  , color: 'red'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $4]]\
    \  , fnType: 'points'\
    \  , color: 'red'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8'\
    \  , nSamples: 101\
    \  , color: 'purple'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $5]]\
    \  , fnType: 'points'\
    \  , color: 'purple'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  },{\
    \    fn: '1 - ((0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8'\
    \  , nSamples: 101\
    \  , color: 'green'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $6]]\
    \  , fnType: 'points'\
    \  , color: 'green'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \, annotations: [{\
    \    x: $2\
    \  , text: 'pilots in goal'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot :: IsElement e => e -> GoalRatio -> Weights -> IO Plot
hgPlot
    e
    (GoalRatio gr)
    Weights
        { distance = DistanceWeight d
        , arrival = ArrivalWeight a
        , leading = LeadingWeight l
        , time = TimeWeight t
        } = do
    gr' <- toJSVal gr
    d' <- toJSVal d
    a' <- toJSVal a
    l' <- toJSVal l
    t' <- toJSVal t

    Plot <$> hgPlot_ (unElement . toElement $ e) gr' d' l' a' t'

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#pg-plot'\
    \, title: 'Split of Available Points'\
    \, width: 360\
    \, height: 400\
    \, disableZoom: true\
    \, xAxis: {label: 'Fraction of Pilots in Goal', domain: [0, 1]}\
    \, yAxis: {domain: [0, 1]}\
    \, data: [{\
    \    fn: '0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3'\
    \  , nSamples: 101\
    \  , color: 'blue'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $3]]\
    \  , fnType: 'points'\
    \  , color: 'blue'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4 * 2'\
    \  , nSamples: 101\
    \  , color: 'red'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $4]]\
    \  , fnType: 'points'\
    \  , color: 'red'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  },{\
    \    fn: '1 - ((0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4 * 2)'\
    \  , nSamples: 101\
    \  , color: 'green'\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$2, $5]]\
    \  , fnType: 'points'\
    \  , color: 'green'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \, annotations: [{\
    \    x: $2\
    \  , text: 'pilots in goal'\
    \  }]\
    \})"
    pgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

pgPlot :: IsElement e => e -> GoalRatio -> Weights -> IO Plot
pgPlot
    e
    (GoalRatio gr)
    Weights
        { distance = DistanceWeight d
        , leading = LeadingWeight l
        , time = TimeWeight t
        } = do
    gr' <- toJSVal gr
    d' <- toJSVal d
    l' <- toJSVal l
    t' <- toJSVal t

    Plot <$> pgPlot_ (unElement . toElement $ e) gr' d' l' t'

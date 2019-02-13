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

import WireTypes.Point (GoalRatio(..))

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot'\
    \, width: 360\
    \, height: 400\
    \, disableZoom: true\
    \, xAxis: {label: 'Fraction of Pilots in Goal', domain: [0, 1]}\
    \, yAxis: {label: 'Fraction of Available Points', domain: [0, 1]}\
    \, data: [{\
    \    fn: '0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3'\
    \  , nSamples: 101\
    \  , color: 'blue'\
    \  , graphType: 'polyline'\
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4'\
    \  , nSamples: 101\
    \  , color: 'red'\
    \  , graphType: 'polyline'\
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8'\
    \  , nSamples: 101\
    \  , color: 'purple'\
    \  , graphType: 'polyline'\
    \  },{\
    \    fn: '1 - ((0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8'\
    \  , nSamples: 101\
    \  , color: 'green'\
    \  , graphType: 'polyline'\
    \  }]\
    \, annotations: [{\
    \    x: $2\
    \  , text: 'pilots in goal'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> IO JSVal

hgPlot :: IsElement e => e -> GoalRatio -> IO Plot
hgPlot e (GoalRatio gr) = do
    gr' <- toJSVal gr
    Plot <$> hgPlot_ (unElement . toElement $ e) gr'

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#pg-plot'\
    \, width: 360\
    \, height: 400\
    \, disableZoom: true\
    \, xAxis: {label: 'Fraction of Pilots in Goal', domain: [0, 1]}\
    \, yAxis: {label: 'Fraction of Available Points', domain: [0, 1]}\
    \, data: [{\
    \    fn: '0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3'\
    \  , nSamples: 101\
    \  , color: 'blue'\
    \  , graphType: 'polyline'\
    \  },{\
    \    fn: '(1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4 * 2'\
    \  , nSamples: 101\
    \  , color: 'red'\
    \  , graphType: 'polyline'\
    \  },{\
    \    fn: '1 - ((0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3) + (1 - (0.9 - 1.665*x + 1.713*x^2 - 0.587*x^3))/8 * 1.4 * 2)'\
    \  , nSamples: 101\
    \  , color: 'green'\
    \  , graphType: 'polyline'\
    \  }]\
    \, annotations: [{\
    \    x: $2\
    \  , text: 'pilots in goal'\
    \  }]\
    \})"
    pgPlot_ :: JSVal -> JSVal -> IO JSVal

pgPlot :: IsElement e => e -> GoalRatio -> IO Plot
pgPlot e (GoalRatio gr) = do
    gr' <- toJSVal gr
    Plot <$> pgPlot_ (unElement . toElement $ e) gr'

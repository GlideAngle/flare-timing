{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Arrival.Plot
    ( Plot(..)
    , hgPlot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)

import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (GoalRatio(..))

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-arrival'\
    \, title: 'Arrival Point Distribution'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: 'Arrival Placing', domain: [0, $2 + 1]}\
    \, yAxis: {domain: [0, 1.01]}\
    \, data: [{\
    \    fn: '0.2 + 0.037*(1 - (x - 1)/' + $2 + ') + 0.13*(1 - (x - 1)/' + $2 + ')^2 + 0.633*(1 - (x - 1)/' + $2 + ')^3'\
    \  , nSamples: 101\
    \  , color: 'blue'\
    \  , range: [1, $2]\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: $3\
    \  , fnType: 'points'\
    \  , color: 'blue'\
    \  , attr: { r: 2 }\
    \  , range: [1, $2]\
    \  , graphType: 'scatter'\
    \  },{\
    \    points: $4\
    \  , fnType: 'points'\
    \  , color: 'red'\
    \  , attr: { r: 3 }\
    \  , range: [1, $2]\
    \  , graphType: 'scatter'\
    \  }]\
    \, annotations: [{\
    \    y: 0.2\
    \  , text: 'minimum possible fraction'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot
    :: IsElement e
    => e
    -> PilotsFlying
    -> GoalRatio
    -> [[Double]]
    -> [[Double]]
    -> IO Plot
hgPlot e (PilotsFlying pf) (GoalRatio gr) xs ys = do
    let n :: Integer = truncate $ gr * fromIntegral pf

    n' <- toJSVal (fromIntegral n :: Double)
    xs' <- toJSValListOf xs
    ys' <- toJSValListOf ys

    Plot <$> hgPlot_ (unElement . toElement $ e) n' xs' ys'

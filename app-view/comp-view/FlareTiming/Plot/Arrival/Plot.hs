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
    \, xAxis: {label: 'Arrival Position', domain: [0, $2 + 1]}\
    \, yAxis: {domain: [0, 1.01]}\
    \, data: [{\
    \    points: $3\
    \  , fnType: 'points'\
    \  , graphType: 'scatter'\
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot :: IsElement e => e -> PilotsFlying -> GoalRatio -> IO Plot
hgPlot e (PilotsFlying pf) (GoalRatio gr) = do
    let n :: Integer = truncate $ gr * fromIntegral pf
    let xy :: [[Double]] = [[fromIntegral x, fn n x] | x <- [1..n]]

    n' <- toJSVal (fromIntegral n :: Double)
    xy' <- toJSValListOf xy

    Plot <$> hgPlot_ (unElement . toElement $ e) n' xy'

fn :: Integer -> Integer -> Double
fn n x' = 0.2 + 0.037 * y + 0.13 * y**2 + 0.633 * y**3
    where
        x :: Double
        x = fromIntegral x'

        y :: Double
        y = 1.0 - (x - 1.0) / (fromIntegral n)

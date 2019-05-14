{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Valid.Plot (launchPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal, toJSValListOf)

import WireTypes.Validity (LaunchValidity(..))
import WireTypes.ValidityWorking
    ( LaunchValidityWorking(..)
    , PilotsFlying(..), PilotsPresent(..), NominalLaunch(..)
    )
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-launch'\
    \, title: 'Launch Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [-0.5, $5 + 0.5]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#000000'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#000000'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \})"
    hgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSString -> IO JSVal

launchPlot :: IsElement e => e -> LaunchValidity -> LaunchValidityWorking -> IO Plot
launchPlot
    e
    (LaunchValidity y)
    LaunchValidityWorking
        { flying = PilotsFlying pf
        , present = PilotsPresent pp
        , nominalLaunch = NominalLaunch nl
        } = do
    let pf' :: Double = fromIntegral pf
    let pp' :: Double = fromIntegral pp

    let xy :: [[Double]] =
            [ [x', fnLaunch d x']
            | x <- [0 .. 10 * pp]
            , let x' = 0.1 * fromIntegral x
            , let d = pp' * nl
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal pf'
    y' <- toJSVal y
    pp'' <- toJSVal pp'
    let msg = "Pilots Flying (" ++ show pf ++ " out of " ++ show pp ++ ")"

    Plot <$> hgPlot_ (unElement . toElement $ e) xy' x' y' pp'' (toJSString msg)

fnLaunch :: Double -> Double -> Double
fnLaunch d n = 0.027 * x + 2.917 * x**2 - 1.944 * x**3
    where
        x = min 1 $ n / d

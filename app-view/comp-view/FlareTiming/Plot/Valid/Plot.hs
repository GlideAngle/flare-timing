{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Valid.Plot
    ( Plot(..)
    , hgPlot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSVal, toJSValListOf)

import WireTypes.Validity (LaunchValidity(..))
import WireTypes.ValidityWorking
    ( LaunchValidityWorking(..)
    , PilotsFlying(..), PilotsPresent(..), NominalLaunch(..)
    )

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-launch'\
    \, title: 'Launch Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: 'Launch Validity Ratio', domain: [-0.05, 1.05]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#000000'\
    \  , range: [0, 1]\
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
    hgPlot_ :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

hgPlot :: IsElement e => e -> LaunchValidity -> LaunchValidityWorking -> IO Plot
hgPlot
    e
    (LaunchValidity y)
    LaunchValidityWorking
        { flying = PilotsFlying pf
        , present = PilotsPresent pp
        , nominalLaunch = NominalLaunch nl
        } = do

    let xy :: [[Double]] =
            [ [x', fn x']
            | x <- [0 :: Integer .. 100]
            , let x' = 0.01 * fromIntegral x
            ]

    xy' <- toJSValListOf xy

    let x = min 1 $ (fromIntegral pf) / (fromIntegral pp * nl)
    x' <- toJSVal x
    y' <- toJSVal y

    Plot <$> hgPlot_ (unElement . toElement $ e) xy' x' y'

fn :: Double -> Double
fn x = 0.027 * x + 2.917 * x**2 - 1.944 * x**3

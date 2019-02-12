{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.FunctionPlot
    ( plot
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal)

import WireTypes.Pilot (PilotName(..))

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

foreign import javascript unsafe
    "functionPlot({target: '#plot', data: [{ fn: 'x^2' }] })"
    plot_ :: JSVal -> IO JSVal

plot :: IsElement e => e -> IO Plot
plot e =
    Plot <$> (plot_ . unElement . toElement $ e)

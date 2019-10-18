module FlareTiming.Plot.Foreign (Plot(..)) where

import GHCJS.Types (JSVal)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Plot = Plot { unPlot :: JSVal }

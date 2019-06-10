module FlareTiming.Statistics
    ( mean
    ) where

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)

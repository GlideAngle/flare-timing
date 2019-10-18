module FlareTiming.Statistics (mean, stdDev) where

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)

stdDev :: [Double] -> Double
stdDev = sqrt . variance

variance :: [Double] -> Double
variance xs =
    (sum $ zipWith (*) ys ys) / (fromIntegral $ length xs)
    where
        xMean = mean xs
        ys = ((-) xMean) <$> xs

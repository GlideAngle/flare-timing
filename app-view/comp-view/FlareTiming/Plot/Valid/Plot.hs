{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Valid.Plot (launchPlot, timePlot, reachPlot) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal, toJSValListOf)

import WireTypes.Validity (LaunchValidity(..), TimeValidity(..), DistanceValidity(..))
import WireTypes.ValidityWorking
    ( LaunchValidityWorking(..), TimeValidityWorking(..), DistanceValidityWorking(..)
    , PilotsFlying(..), PilotsPresent(..), NominalLaunch(..)
    , BestTime(..), NominalTime(..)
    , BestDistance(..), NominalDistance(..)
    , SumOfDistance(..), NominalDistanceArea(..)
    , MinimumDistance(..)
    )
import FlareTiming.Plot.Foreign (Plot(..))

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-launch'\
    \, title: 'Launch Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [0 - $5 * 0.05, $5 * 1.05]}\
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
    plotLaunch_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-time'\
    \, title: 'Time Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [0 - $5 * 0.05, $5 * 1.05]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#4daf4a'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \, annotations: [{\
    \    x: $7\
    \  , text: $8\
    \  }]\
    \})"
    plotTime_
        :: JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSString
        -> JSVal
        -> JSString
        -> IO JSVal

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-reach'\
    \, title: 'Reach Validity'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [0 - $5 * 0.05, $5 * 1.05]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#377eb8'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \})"
    plotReach_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSString -> IO JSVal

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
            | x <- [1 .. 199 * pp]
            , let x' = 0.005 * fromIntegral x
            , let d = pp' * nl
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal pf'
    y' <- toJSVal y
    pp'' <- toJSVal pp'
    let msg = "Pilots Flying (" ++ show pf ++ " out of " ++ show pp ++ ")"

    Plot <$>
        plotLaunch_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            pp''
            (toJSString msg)

fnLaunch :: Double -> Double -> Double
fnLaunch d n = 0.027 * x + 2.917 * x**2 - 1.944 * x**3
    where
        x = min 1 $ n / d

timePlot :: IsElement e => e -> TimeValidity -> TimeValidityWorking -> IO Plot
timePlot e vy TimeValidityWorking{gsBestTime = Just bt, ..} =
    timeTime e vy bt nominalTime
timePlot e vy TimeValidityWorking{ssBestTime = Just bt, ..} =
    timeTime e vy bt nominalTime
timePlot e vy TimeValidityWorking{..} =
    timeDistance e vy bestDistance nominalDistance

timeTime :: IsElement e => e -> TimeValidity -> BestTime -> NominalTime -> IO Plot
timeTime
    e
    (TimeValidity y)
    (BestTime bt)
    (NominalTime nt) = do

    let xMax = max bt nt

    let xy :: [[Double]] =
            [ [x', fnTime nt x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal bt
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Best Time (h)" :: String
    let msgNominal = "Nominal Time" :: String
    nt' <- toJSVal nt

    Plot <$>
        plotTime_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)
            nt'
            (toJSString msgNominal)

timeDistance :: IsElement e => e -> TimeValidity -> BestDistance -> NominalDistance -> IO Plot
timeDistance
    e
    (TimeValidity y)
    (BestDistance bd)
    (NominalDistance nd) = do

    let xMax = max bd nd

    let xy :: [[Double]] =
            [ [x', fnTime nd x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal bd
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Best Distance (km)" :: String
    let msgNominal = "Nominal Distance" :: String
    nd' <- toJSVal nd

    Plot <$>
        plotTime_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)
            nd'
            (toJSString msgNominal)

fnTime :: Double -> Double -> Double
fnTime d n = max 0 $ min 1 $ 0 - 0.271 + 2.912 * x - 2.098 * x**2 + 0.457 * x**3
    where
        x = min 1 $ n / d

reachPlot :: IsElement e => e -> DistanceValidity -> DistanceValidityWorking -> IO Plot
reachPlot
    e
    (DistanceValidity y)
    DistanceValidityWorking
        { sum = SumOfDistance dSum
        , flying = PilotsFlying pf
        , area = NominalDistanceArea nda
        , minimumDistance = dMin@(MinimumDistance dMin')
        } = do
    let pf' :: Double = fromIntegral pf
    let dMean = dSum / pf'
    let xMax = max dMean nda

    let xy :: [[Double]] =
            [ [x' , fnReach dMin nda x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * (xMax + dMin')
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal $ y * nda + dMin'
    y' <- toJSVal y
    xMax' <- toJSVal $ xMax + dMin'
    let msg = "Mean Distance (km)" :: String

    Plot <$>
        plotReach_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)

fnReach :: MinimumDistance -> Double -> Double -> Double
fnReach (MinimumDistance dMin) d n
    | d == 0 = 0
    | n < dMin = 0
    | otherwise = min 1 $ (n - dMin) / d

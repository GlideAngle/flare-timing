{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Plot.Valid.Plot
    ( launchPlot
    , timePlot
    , reachPlot
    , stopByReachPlot
    , stopByLandedPlot
    , stopByVaryPlot
    ) where

import Prelude hiding (min, max, map, log)
import qualified Prelude as Stats (min, max)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal, toJSValListOf)

import WireTypes.Validity
    (LaunchValidity(..), TimeValidity(..), DistanceValidity(..), StopValidity(..))
import WireTypes.ValidityWorking
    ( LaunchValidityWorking(..)
    , TimeValidityWorking(..)
    , DistanceValidityWorking(..)
    , ReachStats(..), StopValidityWorking(..)
    , PilotsFlying(..), PilotsPresent(..), NominalLaunch(..)
    , BestTime(..), NominalTime(..)
    , NominalDistance(..)
    , SumOfDistance(..), NominalDistanceArea(..)
    , MinimumDistance(..)
    , PilotsAtEss(..), PilotsLanded(..), LaunchToEss(..)
    )
import WireTypes.Point (PilotDistance(..), ReachToggle(..))
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
    \  , color: '#ff7f00'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
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
    \, annotations: [{\
    \    x: $7\
    \  , text: $8\
    \  }]\
    \})"
    plotReach_
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
    \{ target: '#hg-plot-valid-stop-by-reach'\
    \, title: 'Stop Validity vs Reach'\
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
    \, annotations: [{\
    \    x: $7\
    \  , text: $8\
    \  }]\
    \})"
    plotStopByReach_
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
    \{ target: '#hg-plot-valid-stop-by-landed'\
    \, title: 'Stop Validity vs Landed'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [0 - $5 * 0.05, $5 * 1.05]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#984ea3'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \})"
    plotStopByLanded_
        :: JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSString
        -> IO JSVal

foreign import javascript unsafe
    "functionPlot(\
    \{ target: '#hg-plot-valid-stop-by-vary'\
    \, title: 'Stop Validity vs Spread'\
    \, width: 360\
    \, height: 360\
    \, disableZoom: true\
    \, xAxis: {label: $6, domain: [0 - $5 * 0.05, $5 * 1.05]}\
    \, yAxis: {domain: [-0.05, 1.05]}\
    \, data: [{\
    \    points: $2\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , range: [0, $5]\
    \  , attr: { stroke-dasharray: '5,5' }\
    \  , graphType: 'polyline'\
    \  },{\
    \    points: [[$3, $4]]\
    \  , fnType: 'points'\
    \  , color: '#ff7f00'\
    \  , attr: { r: 3 }\
    \  , graphType: 'scatter' \
    \  }]\
    \})"
    plotStopByVary_
        :: JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSVal
        -> JSString
        -> IO JSVal

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
        x = Stats.min 1 $ n / d

timePlot :: IsElement e => e -> TimeValidity -> TimeValidityWorking -> IO Plot
timePlot e vy TimeValidityWorking{gsBestTime = Just bt, ..} =
    timeTime e vy bt nominalTime
timePlot e vy TimeValidityWorking{ssBestTime = Just bt, ..} =
    timeTime e vy bt nominalTime
timePlot e vy TimeValidityWorking{reachMax = ReachToggle{extra = bd}, ..} =
    timeDistance e vy bd nominalDistance

timeTime :: IsElement e => e -> TimeValidity -> BestTime -> NominalTime -> IO Plot
timeTime
    e
    (TimeValidity y)
    (BestTime bt)
    (NominalTime nt) = do

    let xMax = Stats.max bt nt

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

timeDistance :: IsElement e => e -> TimeValidity -> PilotDistance -> NominalDistance -> IO Plot
timeDistance
    e
    (TimeValidity y)
    (PilotDistance bd)
    (NominalDistance nd) = do

    let xMax = Stats.max bd nd

    let xy :: [[Double]] =
            [ [x', fnTime nd x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal bd
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Best Flown Distance (km)" :: String
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
fnTime d n =
    Stats.max 0
    $ Stats.min 1
    $ 0 - 0.271 + 2.912 * x - 2.098 * x**2 + 0.457 * x**3
    where
        x = Stats.min 1 $ n / d

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
    let xMax = Stats.max dMean nda

    let xy :: [[Double]] =
            [ [x' , fnReach dMin nda x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * (xMax + dMin')
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal $ y * (if y < 1 then nda else dMean) + dMin'
    y' <- toJSVal y
    xMax' <- toJSVal $ xMax + dMin'
    let msg = "Mean Flown Distance (km)" :: String

    dMin'' <- toJSVal dMin'
    let msgMin = "Minimum Distance" :: String

    Plot <$>
        plotReach_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)
            dMin''
            (toJSString msgMin)

fnReach :: MinimumDistance -> Double -> Double -> Double
fnReach (MinimumDistance dMin) d n
    | d == 0 = 0
    | n < dMin = 0
    | otherwise = Stats.min 1 $ (n - dMin) / d

stopByReachPlot :: IsElement e => e -> StopValidity -> StopValidityWorking -> IO Plot
stopByReachPlot _ _ StopValidityWorking{reachStats = ReachToggle{flown = Nothing}} = error "flown is missing"
stopByReachPlot
    e
    (StopValidity y)
    vw@StopValidityWorking
        { reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = PilotDistance bd
                        , mean = PilotDistance dMean
                        }
                }
        } = do

    let xMax = bd

    let xy :: [[Double]] =
            [ [x' , fnStopByReach vw x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal dMean
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Mean Flown Distance (km)" :: String
    let msgBest = "Best Flown Distance" :: String

    Plot <$>
        plotStopByReach_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)
            xMax'
            (toJSString msgBest)

fnStopByReach :: StopValidityWorking -> Double -> Double
fnStopByReach StopValidityWorking{launchToEssDistance = Nothing} _ = 1
fnStopByReach StopValidityWorking{reachStats = ReachToggle{flown = Nothing}} _ = error "flown is missing"
fnStopByReach
    StopValidityWorking
        { pilotsAtEss = PilotsAtEss ess
        , landed = PilotsLanded landed
        , flying = PilotsFlying flying
        , launchToEssDistance = Just (LaunchToEss ed)
        , reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = PilotDistance bd
                        , stdDev = PilotDistance flownStdDev
                        }
                }
        }
    flownMean
    | ess > 0 = 1
    | otherwise = Stats.min 1 $ a + b**3
        where
            a = sqrt (((bd - flownMean) / (ed - bd + 1)) * sqrt (flownStdDev / 5))
            b = fromIntegral landed / (fromIntegral flying :: Double)

stopByLandedPlot :: IsElement e => e -> StopValidity -> StopValidityWorking -> IO Plot
stopByLandedPlot
    e
    (StopValidity y)
    vw@StopValidityWorking
        { flying = PilotsFlying pf
        , landed = PilotsLanded landedByStop
        } = do

    let xLanded = fromIntegral landedByStop :: Double
    let xMax = fromIntegral pf

    let xy :: [[Double]] =
            [ [x' , fnStopByLanded vw x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal xLanded
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Pilots Landed before Stop" :: String

    Plot <$>
        plotStopByLanded_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)

fnStopByLanded :: StopValidityWorking -> Double -> Double
fnStopByLanded StopValidityWorking{launchToEssDistance = Nothing} _ = 1
fnStopByLanded StopValidityWorking{reachStats = ReachToggle{flown = Nothing}} _ = error "flown is missing"
fnStopByLanded
    StopValidityWorking
        { pilotsAtEss = PilotsAtEss ess
        , flying = PilotsFlying flying
        , launchToEssDistance = Just (LaunchToEss ed)
        , reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = PilotDistance bd
                        , mean = PilotDistance flownMean
                        , stdDev = PilotDistance flownStdDev
                        }
                }
        }
    landed
    | ess > 0 = 1
    | otherwise = Stats.min 1 $ a + b**3
        where
            a = sqrt (((bd - flownMean) / (ed - bd + 1)) * sqrt (flownStdDev / 5))
            b = landed / (fromIntegral flying :: Double)

stopByVaryPlot :: IsElement e => e -> StopValidity -> StopValidityWorking -> IO Plot
stopByVaryPlot _ _ StopValidityWorking{reachStats = ReachToggle{flown = Nothing}} = error "flown is missing"
stopByVaryPlot
    e
    (StopValidity y)
    vw@StopValidityWorking
        { reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = PilotDistance bd
                        , stdDev = PilotDistance std
                        }
                }
        } = do

    let xMax = bd

    let xy :: [[Double]] =
            [ [x' , fnStopByVary vw x']
            | x <- [0 .. 199 :: Integer]
            , let x' = 0.005 * fromIntegral x * xMax
            ]

    xy' <- toJSValListOf xy

    x' <- toJSVal std
    y' <- toJSVal y
    xMax' <- toJSVal xMax
    let msg = "Standard Deviation of Flown Distance (km)" :: String

    Plot <$>
        plotStopByVary_
            (unElement . toElement $ e)
            xy'
            x'
            y'
            xMax'
            (toJSString msg)

fnStopByVary :: StopValidityWorking -> Double -> Double
fnStopByVary StopValidityWorking{launchToEssDistance = Nothing} _ = 1
fnStopByVary StopValidityWorking{reachStats = ReachToggle{flown = Nothing}} _ = error "flown is missing"
fnStopByVary
    StopValidityWorking
        { pilotsAtEss = PilotsAtEss ess
        , landed = PilotsLanded landed
        , flying = PilotsFlying flying
        , launchToEssDistance = Just (LaunchToEss ed)
        , reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = PilotDistance bd
                        , mean = PilotDistance flownMean
                        }
                }
        }
    flownStdDev
    | ess > 0 = 1
    | otherwise = Stats.min 1 $ a + b**3
        where
            a = sqrt (((bd - flownMean) / (ed - bd + 1)) * sqrt (flownStdDev / 5))
            b = fromIntegral landed / (fromIntegral flying :: Double)

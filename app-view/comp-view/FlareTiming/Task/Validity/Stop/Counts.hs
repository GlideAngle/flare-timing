module FlareTiming.Task.Validity.Stop.Counts (viewStopCounts) where

import Prelude hiding (sum)
import qualified Prelude as Stats (sum)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Reflex
import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showStopValidity, showStopValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    , PilotsFlying(..)
    , MaximumDistance(..)
    , showPilotsFlyingDiff
    , showPilotsLandedDiff
    , showPilotsAtEssDiff
    , showBestDistance, showBestDistanceDiff
    , showLaunchToEss, showLaunchToEssDiff
    )
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..), showTaskDistance)
import WireTypes.Reach (TrackReach(..))
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (UtcOffset(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (timeZone, showTime)
import qualified FlareTiming.Statistics as Stats (mean, stdDev)
import FlareTiming.Task.Validity.Widget (katexNewLine, spacer, elV, elN, elD)

viewStopCounts
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> TaskDistance
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
viewStopCounts _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ _ = return ()
viewStopCounts _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ = return ()
viewStopCounts _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ _ = return ()
viewStopCounts _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ = return ()
viewStopCounts
    utcOffset
    Vy.Validity{stop = sv}
    Vy.Validity{stop = svN}
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss
                , flying
                , landed
                , extra =
                    ReachStats
                        { max = extraMax
                        , mean = extraMean
                        , stdDev = extraStdDev
                        }
                , flown =
                    ReachStats
                        { max = flownMax
                        , mean = flownMean
                        , stdDev = flownStdDev
                        }
                , launchToEssDistance = ed
                }
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss = pilotsAtEssN
                , flying = flyingN
                , landed = landedN
                , extra =
                    ReachStats
                        { max = extraMaxN
                        , mean = extraMeanN
                        , stdDev = extraStdDevN
                        }
                , flown =
                    ReachStats
                        { max = flownMaxN
                        , mean = flownMeanN
                        , stdDev = flownStdDevN
                        }
                , launchToEssDistance = edN
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = bolsterMax
                , mean = bolsterMean
                , stdDev = bolsterStdDev
                }
        , reach =
            ReachStats
                { max = reachMax
                , mean = reachMean
                , stdDev = reachStdDev
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = bolsterMaxE
                , mean = bolsterMeanE
                , stdDev = bolsterStdDevE
                }
        , reach =
            ReachStats
                { max = reachMaxE
                , mean = reachMeanE
                , stdDev = reachStdDevE
                }
        }
    reach
    bonusReach
    _td
    landedByStop
    stillFlying = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                elAttr "th" ("colspan" =: "2") $ text ""
                elClass "th" "has-text-right" $ text "Pilots"
                elClass "th" "has-text-right" $ text "Distance"
                elClass "th" "has-text-right" $ text "Validity"
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "td" $ text ""
                el "td" $ text "at ESS"
                elV . T.pack $ show pilotsAtEss
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show pilotsAtEssN
                elD $ showPilotsAtEssDiff pilotsAtEssN pilotsAtEss
                return ()

            el "tr" $ do
                el "td" $ text "f"
                el "td" $ text "Flying"
                elV . T.pack $ show flying
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show flyingN
                elD $ showPilotsFlyingDiff flyingN flying
                return ()

            el "tr" $ do
                el "td" $ text "ls"
                el "td" $ text "Landed before Stop"
                elV . T.pack $ show landed
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show landedN
                elD $ showPilotsLandedDiff landedN landed
                return ()

            el "tr" $ do
                el "td" $ text "ed"
                el "td" $ text "Launch to ESS"
                el "td" $ text ""
                elV $ showLaunchToEss ed
                el "td" $ text ""
                elN $ showLaunchToEss edN
                elD $ showLaunchToEssDiff edN ed
                return ()

            el "tr" $ do
                el "td" $ text ""
                el "th" $ text "Stop"
                el "td" $ text ""
                el "td" $ text ""
                elV $ Vy.showStopValidity sv
                elN $ Vy.showStopValidity svN
                elD $ Vy.showStopValidityDiff svN sv
                return ()

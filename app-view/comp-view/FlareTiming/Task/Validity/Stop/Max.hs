module FlareTiming.Task.Validity.Stop.Max (viewStopMax) where

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

viewStopMax
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
viewStopMax _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ _ = return ()
viewStopMax _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ = return ()
viewStopMax _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ _ = return ()
viewStopMax _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ = return ()
viewStopMax
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
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "td" $ text "Reach †"
                elV $ showBestDistance reachMax
                elN $ showBestDistance flownMaxN
                elD $ showBestDistanceDiff flownMaxN reachMax
                return ()

            el "tr" $ do
                el "th" $ text "Bolster (Flown Max)"
                elV $ showBestDistance flownMax
                elN $ showBestDistance flownMaxN
                elD $ showBestDistanceDiff flownMaxN flownMax
                return ()

            el "tr" $ do
                el "th" $ text "Bolster Max"
                elV $ showBestDistance bolsterMax
                elN $ showBestDistance flownMaxN
                elD $ showBestDistanceDiff flownMaxN bolsterMax
                return ()

            el "tr" $ do
                el "td" $ text "Extra ‖"
                elV $ showBestDistance extraMax
                elN $ showBestDistance extraMaxN
                elD $ showBestDistanceDiff extraMaxN extraMax
                return ()

            el "tr" $ do
                el "td" $ text "Working"
                elV $ showBestDistance flownMax
                elN $ showBestDistance flownMaxN
                elD $ ""
                return ()

            el "tr" $ do
                el "td" $ text "Extra Working"
                elV $ showBestDistance extraMax
                elN $ showBestDistance extraMaxN
                elD $ ""
                return ()

        let tdFoot = elAttr "td" ("colspan" =: "4")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "† Reach as small as actually flown."
            foot "‡ Bolstered, no smaller than minimum distance."
            foot "‖ Extra altitude above goal converted to extra reach via glide."
            return ()

module FlareTiming.Task.Validity.Stop.Mean (viewStopMean) where

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
import WireTypes.Point (PilotDistance(..), showPilotDistance, showPilotDistanceDiff)
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (UtcOffset(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (timeZone, showTime)
import qualified FlareTiming.Statistics as Stats (mean, stdDev)
import FlareTiming.Task.Validity.Widget (katexNewLine, spacer, elV, elN, elD)

viewStopMean
    :: MonadWidget t m
    => ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> m ()
viewStopMean ValidityWorking{stop = Nothing} _ _ _ = return ()
viewStopMean _ ValidityWorking{stop = Nothing} _ _ = return ()
viewStopMean
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { mean = extraMean
                        }
                , flown =
                    ReachStats
                        { mean = flownMean
                        }
                }
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { mean = extraMeanN
                        }
                , flown =
                    ReachStats
                        { mean = flownMeanN
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { mean = _bolsterMean
                }
        , reach =
            ReachStats
                { mean = reachMean
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { mean = _bolsterMeanE
                }
        , reach =
            ReachStats
                { mean = reachMeanE
                }
        }
    = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text ""
                elClass "th" "has-text-right" $ text "Reach"
                elClass "th" "has-text-right" $ text "Bolster"
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "th" $ text "Flown ‖"
                elV $ showPilotDistance 3 reachMean
                -- NOTE: bolsterMean == flownMean
                elV $ showPilotDistance 3 flownMean
                elN $ showPilotDistance 3 flownMeanN
                elD $ showPilotDistanceDiff 3 flownMeanN flownMean
                return ()

            el "tr" $ do
                el "th" $ text "Extra ¶"
                elV $ showPilotDistance 3 reachMeanE
                -- NOTE: bolsterMeanE == extraMean
                elV $ showPilotDistance 3 extraMean
                elN $ showPilotDistance 3 extraMeanN
                elD $ showPilotDistanceDiff 3 extraMeanN extraMean
                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "‖ As flown without extra."
            foot "¶ Extra altitude above goal converted to extra reach via glide."
            return ()

module FlareTiming.Task.Validity.Stop.StdDev (viewStopStdDev) where

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

viewStopStdDev
    :: MonadWidget t m
    => ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> m ()
viewStopStdDev ValidityWorking{stop = Nothing} _ _ _ = return ()
viewStopStdDev _ ValidityWorking{stop = Nothing} _ _ = return ()
viewStopStdDev
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { stdDev = extraStdDev
                        }
                , flown =
                    ReachStats
                        { stdDev = flownStdDev
                        }
                }
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { stdDev = extraStdDevN
                        }
                , flown =
                    ReachStats
                        { stdDev = flownStdDevN
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { stdDev = _bolsterStdDev
                }
        , reach =
            ReachStats
                { stdDev = reachStdDev
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { stdDev = _bolsterStdDevE
                }
        , reach =
            ReachStats
                { stdDev = reachStdDevE
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
                el "th" $ text "Flown"
                elV $ showPilotDistance 3 reachStdDev
                -- NOTE: bolsterStdDev == flownStdDev
                elV $ showPilotDistance 3 flownStdDev
                elN $ showPilotDistance 3 flownStdDevN
                elD $ showPilotDistanceDiff 3 flownStdDevN flownStdDev
                return ()

            el "tr" $ do
                el "th" $ text "Extra"
                elV $ showPilotDistance 3 reachStdDevE
                -- NOTE: bolsterStdDevE == extraStdDev
                elV $ showPilotDistance 3 extraStdDev
                elN $ showPilotDistance 3 extraStdDevN
                elD $ showPilotDistanceDiff 3 extraStdDevN extraStdDev
                return ()

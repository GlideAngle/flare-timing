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
    => ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> m ()
viewStopMax ValidityWorking{stop = Nothing} _ _ _ = return ()
viewStopMax _ ValidityWorking{stop = Nothing} _ _ = return ()
viewStopMax
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { max = extraMax
                        }
                , flown =
                    ReachStats
                        { max = flownMax
                        }
                }
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { extra =
                    ReachStats
                        { max = extraMaxN
                        }
                , flown =
                    ReachStats
                        { max = flownMaxN
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = _bolsterMax
                }
        , reach =
            ReachStats
                { max = reachMax
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = _bolsterMaxE
                }
        , reach =
            ReachStats
                { max = reachMaxE
                }
        }
    = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text ""
                elClass "th" "has-text-right" $ text "Reach †"
                elClass "th" "has-text-right" $ text "Bolster ‡"
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "th" $ text "Flown"
                elV $ showBestDistance reachMax
                -- NOTE: bolsterMax == flownMax
                elV $ showBestDistance flownMax
                elN $ showBestDistance flownMaxN
                elD $ showBestDistanceDiff flownMaxN flownMax
                return ()

            el "tr" $ do
                el "th" $ text "Extra"
                elV $ showBestDistance reachMaxE
                -- NOTE: bolsterMaxE == extraMax
                elV $ showBestDistance extraMax
                elN $ showBestDistance extraMaxN
                elD $ showBestDistanceDiff extraMaxN extraMax
                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "† Raw reach, unbolstered."
            foot "‡ Bolstered, no smaller than minimum distance."
            return ()

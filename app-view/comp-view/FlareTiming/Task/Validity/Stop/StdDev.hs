module FlareTiming.Task.Validity.Stop.StdDev (viewStopStdDev) where

import Prelude hiding (sum)
import Reflex.Dom

import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    )
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import WireTypes.Point (showPilotDistance, showPilotDistanceDiff)
import FlareTiming.Task.Validity.Widget (elV, elN, elD)

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
                elClass "th" "th-valid-reach-col" $ text "Reach"
                elClass "th" "th-valid-bolster-col" $ text "Bolster"
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
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 reachStdDevE

                -- NOTE: bolsterStdDevE == extraStdDev
                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 extraStdDev

                elN $ showPilotDistance 3 extraStdDevN
                elD $ showPilotDistanceDiff 3 extraStdDevN extraStdDev
                return ()

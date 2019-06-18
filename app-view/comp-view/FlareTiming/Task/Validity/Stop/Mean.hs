module FlareTiming.Task.Validity.Stop.Mean (viewStopMean) where

import Prelude hiding (sum)
import Reflex.Dom

import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , ReachToggle(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    )
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import WireTypes.Point (showPilotDistance, showPilotDistanceDiff)
import FlareTiming.Task.Validity.Widget (elV, elN, elD)

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
                { reachStats =
                    ReachToggle
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
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { reachStats =
                    ReachToggle
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
                elClass "th" "th-valid-reach-col" $ text "Reach"
                elClass "th" "th-valid-bolster-col" $ text "Bolster"
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
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 reachMeanE

                -- NOTE: bolsterMeanE == extraMean
                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 extraMean

                elN $ showPilotDistance 3 extraMeanN
                elD $ showPilotDistanceDiff 3 extraMeanN extraMean
                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "‖ As flown without extra."
            foot "¶ Glide down to goal altitude for extra reach."
            return ()

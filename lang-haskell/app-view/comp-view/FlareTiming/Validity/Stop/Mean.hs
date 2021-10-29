module FlareTiming.Validity.Stop.Mean (viewStopMean) where

import Prelude hiding (sum)
import Reflex.Dom

import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    )
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import WireTypes.Point (ReachToggle(..), showPilotDistance, showPilotDistanceDiff)
import FlareTiming.Validity.Widget (elV, elN, elD, elVSelect, elNSelect)

viewStopMean
    :: MonadWidget t m
    => ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> m ()
viewStopMean ValidityWorking{stop = Nothing} _ _ _ = return ()
viewStopMean _ ValidityWorking{stop = Nothing} _ _ = return ()
viewStopMean ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{extra = Nothing}}} _ _ _ = return ()
viewStopMean ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{flown = Nothing}}} _ _ _ = return ()
viewStopMean _ ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{extra = Nothing}}} _ _ = return ()
viewStopMean _ ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{flown = Nothing}}} _ _ = return ()
viewStopMean _ _ Stats.BolsterStats{bolster = Nothing} _ = return ()
viewStopMean _ _ Stats.BolsterStats{reach = Nothing} _ = return ()
viewStopMean _ _ _ Stats.BolsterStats{bolster = Nothing} = return ()
viewStopMean _ _ _ Stats.BolsterStats{reach = Nothing} = return ()
viewStopMean
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { reachStats =
                    ReachToggle
                        { extra =
                            Just
                            ReachStats
                                { mean = meanE
                                }
                        , flown =
                            Just
                            ReachStats
                                { mean = meanF
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
                            Just
                            ReachStats
                                { mean = meanEN
                                }
                        , flown =
                            Just
                            ReachStats
                                { mean = meanFN
                                }
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            Just
            ReachStats
                { mean = meanB
                }
        , reach =
            Just
            ReachStats
                { mean = _meanF
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            Just
            ReachStats
                { mean = meanBE
                }
        , reach =
            Just
            ReachStats
                { mean = _meanE
                }
        }
    = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text ""
                elClass "th" "th-valid-reach-col" $ text "Reach"

                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-valid-bolster-col" $ text "Bolster"

        el "tbody" $ do
            el "tr" $ do
                el "th" $ text "Flown ‖"
                elVSelect $ showPilotDistance 3 meanF

                elNSelect $ showPilotDistance 3 meanFN
                elD $ showPilotDistanceDiff 3 meanFN meanF

                elV $ showPilotDistance 3 meanB
                return ()

            el "tr" $ do
                el "th" $ text "Extra ¶"
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 meanE

                elN $ showPilotDistance 3 meanEN
                elD $ showPilotDistanceDiff 3 meanEN meanE

                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 meanBE

                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "‖ As flown without extra."
            foot "¶ Glide down to goal altitude for extra reach."
            return ()

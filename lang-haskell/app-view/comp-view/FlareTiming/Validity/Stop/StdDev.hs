module FlareTiming.Validity.Stop.StdDev (viewStopStdDev) where

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

viewStopStdDev
    :: MonadWidget t m
    => ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> m ()
viewStopStdDev ValidityWorking{stop = Nothing} _ _ _ = return ()
viewStopStdDev _ ValidityWorking{stop = Nothing} _ _ = return ()
viewStopStdDev ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{extra = Nothing}}} _ _ _ = return ()
viewStopStdDev ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{flown = Nothing}}} _ _ _ = return ()
viewStopStdDev _ ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{extra = Nothing}}} _ _ = return ()
viewStopStdDev _ ValidityWorking{stop = Just StopValidityWorking{reachStats = ReachToggle{flown = Nothing}}} _ _ = return ()
viewStopStdDev _ _ Stats.BolsterStats{bolster = Nothing} _ = return ()
viewStopStdDev _ _ Stats.BolsterStats{reach = Nothing} _ = return ()
viewStopStdDev _ _ _ Stats.BolsterStats{bolster = Nothing} = return ()
viewStopStdDev _ _ _ Stats.BolsterStats{reach = Nothing} = return ()
viewStopStdDev
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { reachStats =
                    ReachToggle
                        { extra =
                            Just
                            ReachStats
                                { stdDev = sdE
                                }
                        , flown =
                            Just
                            ReachStats
                                { stdDev = sdF
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
                                { stdDev = sdEN
                                }
                        , flown =
                            Just
                            ReachStats
                                { stdDev = sdFN
                                }
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            Just
            ReachStats
                { stdDev = sdB
                }
        , reach =
            Just
            ReachStats
                { stdDev = _sdF
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            Just
            ReachStats
                { stdDev = sdBE
                }
        , reach =
            Just
            ReachStats
                { stdDev = _sdE
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
                el "th" $ text "Flown"
                elVSelect $ showPilotDistance 3 sdF

                elNSelect $ showPilotDistance 3 sdFN
                elD $ showPilotDistanceDiff 3 sdFN sdF

                elV $ showPilotDistance 3 sdB
                return ()

            el "tr" $ do
                el "th" $ text "Extra"
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 sdE

                elN $ showPilotDistance 3 sdEN
                elD $ showPilotDistanceDiff 3 sdEN sdE

                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 sdBE

                return ()

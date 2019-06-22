module FlareTiming.Task.Validity.Stop.Max (viewStopMax) where

import Prelude hiding (sum)
import Reflex.Dom

import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    )
import WireTypes.Point (ReachToggle(..), showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import FlareTiming.Task.Validity.Widget (elV, elN, elD, elVSelect, elNSelect)

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
                { reachStats =
                    ReachToggle
                        { extra =
                            ReachStats
                                { max = maxE
                                }
                        , flown =
                            ReachStats
                                { max = maxF
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
                                { max = maxEN
                                }
                        , flown =
                            ReachStats
                                { max = maxFN
                                }
                        }
                }
        }
    -- | Reach as flown.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = maxB
                }
        , reach =
            ReachStats
                { max = _maxF
                }
        }
    -- | With extra altitude converted by way of glide to extra reach.
    Stats.BolsterStats
        { bolster =
            ReachStats
                { max = maxBE
                }
        , reach =
            ReachStats
                { max = _maxE
                }
        }
    = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text ""
                elClass "th" "th-valid-reach-col" $ text "Reach †"

                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-valid-bolster-col" $ text "Bolster ‡"

        el "tbody" $ do
            el "tr" $ do
                el "th" $ text "Flown"
                elVSelect $ showPilotDistance 3 maxF

                elNSelect $ showPilotDistance 3 maxFN
                elD $ showPilotDistanceDiff 3 maxFN maxF

                elV $ showPilotDistance 3 maxB
                return ()

            el "tr" $ do
                el "th" $ text "Extra"
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 maxE

                elN $ showPilotDistance 3 maxEN
                elD $ showPilotDistanceDiff 3 maxEN maxE

                -- NOTE: bolsterMaxE == extraMax
                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 maxBE

                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "† Raw reach, unbolstered."
            foot "‡ Bolstered, no smaller than minimum distance."
            return ()

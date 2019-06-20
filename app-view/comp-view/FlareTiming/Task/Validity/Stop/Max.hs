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
import FlareTiming.Task.Validity.Widget (elV, elN, elD)

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
                                { max = extraMax
                                }
                        , flown =
                            ReachStats
                                { max = flownMax
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
                                { max = extraMaxN
                                }
                        , flown =
                            ReachStats
                                { max = flownMaxN
                                }
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
                elClass "th" "th-valid-reach-col" $ text "Reach †"
                elClass "th" "th-valid-bolster-col" $ text "Bolster ‡"
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "th" $ text "Flown"
                elV $ showPilotDistance 3 reachMax
                -- NOTE: bolsterMax == flownMax
                elV $ showPilotDistance 3 flownMax
                elN $ showPilotDistance 3 flownMaxN
                elD $ showPilotDistanceDiff 3 flownMaxN flownMax
                return ()

            el "tr" $ do
                el "th" $ text "Extra"
                elClass "td" "td-valid-reach-extra" . text
                    $ showPilotDistance 3 reachMaxE

                -- NOTE: bolsterMaxE == extraMax
                elClass "td" "td-valid-bolster-extra" . text
                    $ showPilotDistance 3 extraMax

                elN $ showPilotDistance 3 extraMaxN
                elD $ showPilotDistanceDiff 3 extraMaxN extraMax
                return ()

        let tdFoot = elAttr "td" ("colspan" =: "5")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "† Raw reach, unbolstered."
            foot "‡ Bolstered, no smaller than minimum distance."
            return ()

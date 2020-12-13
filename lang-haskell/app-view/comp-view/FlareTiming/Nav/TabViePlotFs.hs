module FlareTiming.Nav.TabViePlotFs (ViePlotFsTab(..), tabsViePlotFs) where

import Reflex
import Reflex.Dom

data ViePlotFsTab
    = ViePlotFsTabReach
    | ViePlotFsTabEffort
    | ViePlotFsTabTime
    | ViePlotFsTabLead
    | ViePlotFsTabArrive

tabsViePlotFs :: MonadWidget t m => m (Event t ViePlotFsTab)
tabsViePlotFs =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (reach, _) <- elDynClass' "li" reachClass . el "a" $ do
                            elClass "span" "legend-reach" $ text "▩"
                            text "Reach"

            (effort, _) <- elDynClass' "li" effortClass . el "a" $ do
                            elClass "span" "legend-effort" $ text "▩"
                            text "Effort"

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            (lead, _) <- elDynClass' "li" leadClass . el "a" $ do
                            elClass "span" "legend-leading" $ text "▩"
                            text "Lead"

            (arrive, _) <- elDynClass' "li" arriveClass . el "a" $ do
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Arrival"

            let eReach = (const ViePlotFsTabReach) <$> domEvent Click reach
            let eEffort = (const ViePlotFsTabEffort) <$> domEvent Click effort
            let eTime = (const ViePlotFsTabTime) <$> domEvent Click time
            let eLead = (const ViePlotFsTabLead) <$> domEvent Click lead
            let eArrive = (const ViePlotFsTabArrive) <$> domEvent Click arrive

            reachClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eReach
                            , "is-active" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eReach
                            , "" <$ eEffort
                            , "is-active" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            leadClass <- holdDyn "" . leftmost $
                            [ "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "is-active" <$ eLead
                            , "" <$ eArrive
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "is-active" <$ eArrive
                            ]

            return . leftmost $
                [ eReach
                , eEffort
                , eTime
                , eLead
                , eArrive
                ]

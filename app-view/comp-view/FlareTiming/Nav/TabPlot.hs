module FlareTiming.Nav.TabPlot (PlotTab(..), tabsPlot) where

import Reflex
import Reflex.Dom

data PlotTab
    = PlotTabSplit
    | PlotTabReach
    | PlotTabEffort
    | PlotTabTime
    | PlotTabLead
    | PlotTabArrive

tabsPlot
    :: MonadWidget t m
    => m (Event t PlotTab)
tabsPlot =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (split, _) <- elDynClass' "li" splitClass . el "a" $ do
                            elClass "span" "legend-reach" $ text "▩"
                            elClass "span" "legend-effort" $ text "▩"
                            elClass "span" "legend-time" $ text "▩"
                            elClass "span" "legend-leading" $ text "▩"
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Split"

            (reach, _) <- elDynClass' "li" reachClass . el "a" $ do
                            elClass "span" "legend-reach" $ text "▩"
                            text "Reach"

            (effort, _) <- elDynClass' "li" effortClass . el "a" $ do
                            elClass "span" "legend-efort" $ text "▩"
                            text "Effort"

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            (lead, _) <- elDynClass' "li" leadClass . el "a" $ do
                            elClass "span" "legend-leading" $ text "▩"
                            text "Lead"

            (arrive, _) <- elDynClass' "li" arriveClass . el "a" $ do
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Arrive"

            let eSplit = (const PlotTabSplit) <$> domEvent Click split
            let eReach = (const PlotTabReach) <$> domEvent Click reach
            let eEffort = (const PlotTabEffort) <$> domEvent Click effort
            let eTime = (const PlotTabTime) <$> domEvent Click time
            let eLead = (const PlotTabLead) <$> domEvent Click lead
            let eArrive = (const PlotTabArrive) <$> domEvent Click arrive

            splitClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "is-active" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "is-active" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "is-active" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            leadClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "is-active" <$ eLead
                            , "" <$ eArrive
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eTime
                            , "" <$ eLead
                            , "is-active" <$ eArrive
                            ]

            return . leftmost $
                [ eSplit
                , eReach
                , eEffort
                , eTime
                , eLead
                , eArrive
                ]

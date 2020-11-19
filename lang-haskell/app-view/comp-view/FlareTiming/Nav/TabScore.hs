module FlareTiming.Nav.TabScore (ScoreTab(..), tabsScore) where

import Reflex
import Reflex.Dom

data ScoreTab
    = ScoreTabOver
    | ScoreTabPenal
    | ScoreTabSplit
    | ScoreTabReach
    | ScoreTabEffort
    | ScoreTabSpeed
    | ScoreTabTime
    | ScoreTabArrive

tabsScore
    :: MonadWidget t m
    => m (Event t ScoreTab)
tabsScore =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (over, _) <- elDynClass' "li" overClass $ el "a" (text "Overview")

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
                            elClass "span" "legend-effort" $ text "▩"
                            text "Effort"

            (speed, _) <- elDynClass' "li" speedClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Speed"

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            (arrive, _) <- elDynClass' "li" arriveClass . el "a" $ do
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Arrival"

            (penal, _) <- elDynClass' "li" penalClass $ el "a" (text "Penal")

            let eOver = (const ScoreTabOver) <$> domEvent Click over
            let eSplit = (const ScoreTabSplit) <$> domEvent Click split
            let eReach = (const ScoreTabReach) <$> domEvent Click reach
            let eEffort = (const ScoreTabEffort) <$> domEvent Click effort
            let eSpeed = (const ScoreTabSpeed) <$> domEvent Click speed
            let eTime = (const ScoreTabTime) <$> domEvent Click time
            let eArrive = (const ScoreTabArrive) <$> domEvent Click arrive
            let ePenal = (const ScoreTabPenal) <$> domEvent Click penal

            overClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            splitClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "is-active" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "is-active" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "is-active" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            speedClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "is-active" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "is-active" <$ eTime
                            , "" <$ eArrive
                            , "" <$ ePenal
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "is-active" <$ eArrive
                            , "" <$ ePenal
                            ]

            penalClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            , "is-active" <$ ePenal
                            ]

            return . leftmost $
                [ eOver
                , eSplit
                , eReach
                , eEffort
                , eSpeed
                , eTime
                , eArrive
                , ePenal
                ]

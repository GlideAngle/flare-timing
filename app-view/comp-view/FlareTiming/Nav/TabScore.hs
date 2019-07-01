module FlareTiming.Nav.TabScore (ScoreTab(..), tabsScore) where

import Reflex
import Reflex.Dom

data ScoreTab
    = ScoreTabOver
    | ScoreTabSplit
    | ScoreTabTime
    | ScoreTabReach
    | ScoreTabEffort

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

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            (reach, _) <- elDynClass' "li" reachClass . el "a" $ do
                            elClass "span" "legend-reach" $ text "▩"
                            text "Reach"

            (effort, _) <- elDynClass' "li" effortClass . el "a" $ do
                            elClass "span" "legend-effort" $ text "▩"
                            text "Effort"

            let eOver = (const ScoreTabOver) <$> domEvent Click over
            let eSplit = (const ScoreTabSplit) <$> domEvent Click split
            let eTime = (const ScoreTabTime) <$> domEvent Click time
            let eReach = (const ScoreTabReach) <$> domEvent Click reach
            let eEffort = (const ScoreTabEffort) <$> domEvent Click effort

            overClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            splitClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "is-active" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "is-active" <$ eTime
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "is-active" <$ eReach
                            , "" <$ eEffort
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eReach
                            , "is-active" <$ eEffort
                            ]

            return . leftmost $
                [ eOver
                , eSplit
                , eTime
                , eReach
                , eEffort
                ]

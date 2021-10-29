module FlareTiming.Nav.TabVieScoreFs (VieScoreFsTab(..), tabsVieScoreFs) where

import Reflex
import Reflex.Dom

data VieScoreFsTab
    = VieScoreFsTabSplit
    | VieScoreFsTabReach
    | VieScoreFsTabEffort
    | VieScoreFsTabSpeed
    | VieScoreFsTabTime
    | VieScoreFsTabArrive

tabsVieScoreFs :: MonadWidget t m => m (Event t VieScoreFsTab)
tabsVieScoreFs =
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

            let eSplit = (const VieScoreFsTabSplit) <$> domEvent Click split
            let eReach = (const VieScoreFsTabReach) <$> domEvent Click reach
            let eEffort = (const VieScoreFsTabEffort) <$> domEvent Click effort
            let eSpeed = (const VieScoreFsTabSpeed) <$> domEvent Click speed
            let eTime = (const VieScoreFsTabTime) <$> domEvent Click time
            let eArrive = (const VieScoreFsTabArrive) <$> domEvent Click arrive

            splitClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "is-active" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "is-active" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            ]

            speedClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "is-active" <$ eSpeed
                            , "" <$ eTime
                            , "" <$ eArrive
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "is-active" <$ eTime
                            , "" <$ eArrive
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eSplit
                            , "" <$ eReach
                            , "" <$ eEffort
                            , "" <$ eSpeed
                            , "" <$ eTime
                            , "is-active" <$ eArrive
                            ]

            return . leftmost $
                [ eSplit
                , eReach
                , eEffort
                , eSpeed
                , eTime
                , eArrive
                ]

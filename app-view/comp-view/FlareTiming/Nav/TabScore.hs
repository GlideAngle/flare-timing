module FlareTiming.Nav.TabScore (ScoreTab(..), tabsScore) where

import Reflex
import Reflex.Dom

data ScoreTab
    = ScoreTabOver
    | ScoreTabPoint
    | ScoreTabSpeed
    | ScoreTabReach
    | ScoreTabEffort

tabsScore
    :: MonadWidget t m
    => m (Event t ScoreTab)
tabsScore =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (over, _) <- elDynClass' "li" overClass $ el "a" (text "Overview")
            (point, _) <- elDynClass' "li" pointClass $ el "a" (text "Points")
            (speed, _) <- elDynClass' "li" speedClass $ el "a" (text "Speed")
            (reach, _) <- elDynClass' "li" reachClass $ el "a" (text "Reach")
            (effort, _) <- elDynClass' "li" effortClass $ el "a" (text "Effort")

            let eOver = (const ScoreTabOver) <$> domEvent Click over
            let ePoint = (const ScoreTabPoint) <$> domEvent Click point
            let eSpeed = (const ScoreTabSpeed) <$> domEvent Click speed
            let eReach = (const ScoreTabReach) <$> domEvent Click reach
            let eEffort = (const ScoreTabEffort) <$> domEvent Click effort

            overClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eOver
                            , "" <$ ePoint
                            , "" <$ eSpeed
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            pointClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "is-active" <$ ePoint
                            , "" <$ eSpeed
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            speedClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ ePoint
                            , "is-active" <$ eSpeed
                            , "" <$ eReach
                            , "" <$ eEffort
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ ePoint
                            , "" <$ eSpeed
                            , "is-active" <$ eReach
                            , "" <$ eEffort
                            ]

            effortClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ ePoint
                            , "" <$ eSpeed
                            , "" <$ eReach
                            , "is-active" <$ eEffort
                            ]

            return . leftmost $
                [ eOver
                , ePoint
                , eSpeed
                , eReach
                , eEffort
                ]

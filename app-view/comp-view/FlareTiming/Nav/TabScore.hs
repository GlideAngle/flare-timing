module FlareTiming.Nav.TabScore (ScoreTab(..), tabsScore) where

import Reflex
import Reflex.Dom

data ScoreTab
    = ScoreTabOver
    | ScoreTabPoint
    | ScoreTabSpeed
    | ScoreTabDistance

tabsScore
    :: MonadWidget t m
    => m (Event t ScoreTab)
tabsScore =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (over, _) <- elDynClass' "li" overClass $ el "a" (text "Overview")
            (point, _) <- elDynClass' "li" pointClass $ el "a" (text "Points")
            (speed, _) <- elDynClass' "li" speedClass $ el "a" (text "Speed")
            (dist, _) <- elDynClass' "li" distClass $ el "a" (text "Distance")

            let eOver = (const ScoreTabOver) <$> domEvent Click over
            let ePoint = (const ScoreTabPoint) <$> domEvent Click point
            let eSpeed = (const ScoreTabSpeed) <$> domEvent Click speed
            let eDist = (const ScoreTabDistance) <$> domEvent Click dist

            overClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eOver
                            , "" <$ ePoint
                            , "" <$ eSpeed
                            , "" <$ eDist
                            ]

            pointClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eOver
                            , "is-active" <$ ePoint
                            , "" <$ eSpeed
                            , "" <$ eDist
                            ]

            speedClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ ePoint
                            , "is-active" <$ eSpeed
                            , "" <$ eDist
                            ]

            distClass <- holdDyn "" . leftmost $
                            [ "" <$ eOver
                            , "" <$ ePoint
                            , "" <$ eSpeed
                            , "is-active" <$ eDist
                            ]

            return . leftmost $
                [ eOver
                , ePoint
                , eSpeed
                , eDist
                ]

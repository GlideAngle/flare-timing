module FlareTiming.Nav.TabPenal (PenalTab(..), tabsPenal) where

import Reflex
import Reflex.Dom

data PenalTab
    = PenalTabJump
    | PenalTabEssGoal
    | PenalTabManual

tabsPenal :: MonadWidget t m => m (Event t PenalTab)
tabsPenal =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (jump, _) <- elDynClass' "li" jumpClass $ el "a" (text "Jump-the-Gun & Auto")
            (essGoal, _) <- elDynClass' "li" essGoalClass $ el "a" (text "Ess ≠ Goal")
            (manual, _) <- elDynClass' "li" otherClass $ el "a" (text "Manual")

            let eJump = (const PenalTabJump) <$> domEvent Click jump
            let eEssGoal = (const PenalTabEssGoal) <$> domEvent Click essGoal
            let eManual = (const PenalTabManual) <$> domEvent Click manual

            jumpClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eJump
                            , "" <$ eEssGoal
                            , "" <$ eManual
                            ]

            essGoalClass <- holdDyn "" . leftmost $
                            [ "" <$ eJump
                            , "is-active" <$ eEssGoal
                            , "" <$ eManual
                            ]

            otherClass <- holdDyn "" . leftmost $
                            [ "" <$ eJump
                            , "" <$ eEssGoal
                            , "is-active" <$ eManual
                            ]

            return . leftmost $
                [ eJump
                , eEssGoal
                , eManual
                ]

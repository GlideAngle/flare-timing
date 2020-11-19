module FlareTiming.Nav.TabPenal (PenalTab(..), tabsPenal) where

import Reflex
import Reflex.Dom

data PenalTab
    = PenalTabJump
    | PenalTabEssGoal
    | PenalTabOther

tabsPenal :: MonadWidget t m => m (Event t PenalTab)
tabsPenal =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (jump, _) <- elDynClass' "li" jumpClass $ el "a" (text "Jump the Gun")
            (essGoal, _) <- elDynClass' "li" essGoalClass $ el "a" (text "Ess ≠ Goal")
            (other, _) <- elDynClass' "li" otherClass $ el "a" (text "Other Penalties")

            let eJump = (const PenalTabJump) <$> domEvent Click jump
            let eEssGoal = (const PenalTabEssGoal) <$> domEvent Click essGoal
            let eOther = (const PenalTabOther) <$> domEvent Click other

            jumpClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eJump
                            , "" <$ eEssGoal
                            , "" <$ eOther
                            ]

            essGoalClass <- holdDyn "" . leftmost $
                            [ "" <$ eJump
                            , "is-active" <$ eEssGoal
                            , "" <$ eOther
                            ]

            otherClass <- holdDyn "" . leftmost $
                            [ "" <$ eJump
                            , "" <$ eEssGoal
                            , "is-active" <$ eOther
                            ]

            return . leftmost $
                [ eJump
                , eEssGoal
                , eOther
                ]

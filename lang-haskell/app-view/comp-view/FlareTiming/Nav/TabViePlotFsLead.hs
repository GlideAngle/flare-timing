module FlareTiming.Nav.TabViePlotFsLead (ViePlotFsLeadTab(..), tabsViePlotFsLead) where

import Reflex
import Reflex.Dom

data ViePlotFsLeadTab
    = ViePlotFsLeadTabPoint
    | ViePlotFsLeadTabArea

tabsViePlotFsLead :: MonadWidget t m => m (Event t ViePlotFsLeadTab)
tabsViePlotFsLead =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (point, _) <- elDynClass' "li" splitClass . el "a" $ do
                            text "Coef:Frac"

            (area, _) <- elDynClass' "li" reachClass . el "a" $ do
                            text "Area:Coef"

            let ePoint = (const ViePlotFsLeadTabPoint) <$> domEvent Click point
            let eArea = (const ViePlotFsLeadTabArea) <$> domEvent Click area

            splitClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ ePoint
                            , "" <$ eArea
                            ]

            reachClass <- holdDyn "" . leftmost $
                            [ "" <$ ePoint
                            , "is-active" <$ eArea
                            ]

            return . leftmost $
                [ ePoint
                , eArea
                ]

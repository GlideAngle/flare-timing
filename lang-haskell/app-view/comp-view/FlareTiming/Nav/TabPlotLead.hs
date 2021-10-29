module FlareTiming.Nav.TabPlotLead (PlotLeadTab(..), tabsPlotLead) where

import Reflex
import Reflex.Dom

data PlotLeadTab
    = PlotLeadTabPoint
    | PlotLeadTabArea

tabsPlotLead :: MonadWidget t m => m (Event t PlotLeadTab)
tabsPlotLead =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (point, _) <- elDynClass' "li" splitClass . el "a" $ do
                            text "Coef:Frac"

            (area, _) <- elDynClass' "li" reachClass . el "a" $ do
                            text "Area:Coef"

            let ePoint = (const PlotLeadTabPoint) <$> domEvent Click point
            let eArea = (const PlotLeadTabArea) <$> domEvent Click area

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

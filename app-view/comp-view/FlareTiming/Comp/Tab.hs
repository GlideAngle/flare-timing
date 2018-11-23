module FlareTiming.Comp.Tab (CompTab(..), tabsComp) where

import Reflex
import Reflex.Dom

data CompTab
    = CompTabTask
    | CompTabPilot

tabsComp
    :: MonadWidget t m
    => m (Event t CompTab)
tabsComp =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Tasks")
            (p, _) <- elDynClass' "li" pClass $ el "a" (text "Pilots")

            let et = (const CompTabTask) <$> domEvent Click t
            let ep = (const CompTabPilot) <$> domEvent Click p

            tClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ et
                            , "" <$ ep
                            ]

            pClass <- holdDyn "" . leftmost $
                            [ "" <$ et
                            , "is-active" <$ ep
                            ]

            return . leftmost $ [et, ep]

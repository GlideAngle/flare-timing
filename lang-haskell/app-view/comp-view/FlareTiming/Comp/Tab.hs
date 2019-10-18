module FlareTiming.Comp.Tab (CompTab(..), tabsComp) where

import Reflex
import Reflex.Dom

data CompTab
    = CompTabSettings
    | CompTabTask
    | CompTabPilot

tabsComp
    :: MonadWidget t m
    => m (Event t CompTab)
tabsComp =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (s, _) <- elDynClass' "li" sClass $ el "a" (text "Settings")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Tasks")
            (p, _) <- elDynClass' "li" pClass $ el "a" (text "Pilots")

            let es = (const CompTabSettings) <$> domEvent Click s
            let et = (const CompTabTask) <$> domEvent Click t
            let ep = (const CompTabPilot) <$> domEvent Click p

            sClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ es
                            , "" <$ et
                            , "" <$ ep
                            ]

            tClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ es
                            , "is-active" <$ et
                            , "" <$ ep
                            ]

            pClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ et
                            , "is-active" <$ ep
                            ]

            return . leftmost $ [es, et, ep]

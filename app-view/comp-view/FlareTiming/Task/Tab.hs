module FlareTiming.Task.Tab (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabGeo
    | TaskTabTask
    | TaskTabMap
    | TaskTabAbsent
    | TaskTabValidity
    | TaskTabScore
    | TaskTabSplit

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (g, _) <- elDynClass' "li" gClass $ el "a" (text "Geo")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Task")
            (m, _) <- elDynClass' "li" mClass $ el "a" (text "Map")
            (a, _) <- elDynClass' "li" aClass $ el "a" (text "Pilots")
            (v, _) <- elDynClass' "li" vClass $ el "a" (text "Valid")
            (s, _) <- elDynClass' "li" sClass $ el "a" (text "Score")
            (p, _) <- elDynClass' "li" pClass $ el "a" (text "Split")

            let eg = (const TaskTabGeo) <$> domEvent Click g
            let et = (const TaskTabTask) <$> domEvent Click t
            let em = (const TaskTabMap) <$> domEvent Click m
            let ea = (const TaskTabAbsent) <$> domEvent Click a
            let ev = (const TaskTabValidity) <$> domEvent Click v
            let es = (const TaskTabScore) <$> domEvent Click s
            let ep = (const TaskTabSplit) <$> domEvent Click p

            gClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eg
                            , "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            , "" <$ ep
                            ]

            tClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eg
                            , "" <$ ev
                            , "" <$ es
                            , "is-active" <$ et
                            , "" <$ em
                            , "" <$ ea
                            , "" <$ ep
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ eg
                            , "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "is-active" <$ em
                            , "" <$ ea
                            , "" <$ ep
                            ]

            aClass <- holdDyn "" . leftmost $
                            [ "" <$ eg
                            , "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "is-active" <$ ea
                            , "" <$ ep
                            ]

            vClass <- holdDyn "" . leftmost $
                            [ "" <$ eg
                            , "is-active" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            , "" <$ ep
                            ]

            sClass <- holdDyn "" . leftmost $
                            [ "" <$ eg
                            , "" <$ ev
                            , "is-active" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            , "" <$ ep
                            ]

            pClass <- holdDyn "" . leftmost $
                            [ "" <$ eg
                            , "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            , "is-active" <$ ep
                            ]

            return . leftmost $ [eg, et, em, ea, ev, es, ep]

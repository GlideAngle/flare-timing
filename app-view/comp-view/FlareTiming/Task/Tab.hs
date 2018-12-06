module FlareTiming.Task.Tab (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabValidity
    | TaskTabScore
    | TaskTabTask
    | TaskTabMap
    | TaskTabAbsent

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (v, _) <- elDynClass' "li" vClass $ el "a" (text "Validity")
            (s, _) <- elDynClass' "li" sClass $ el "a" (text "Scores")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Turnpoints")
            (m, _) <- elDynClass' "li" mClass $ el "a" (text "Map")
            (a, _) <- elDynClass' "li" aClass $ el "a" (text "Absentees")

            let ev = (const TaskTabValidity) <$> domEvent Click v
            let es = (const TaskTabScore) <$> domEvent Click s
            let et = (const TaskTabTask) <$> domEvent Click t
            let em = (const TaskTabMap) <$> domEvent Click m
            let ea = (const TaskTabAbsent) <$> domEvent Click a

            vClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            sClass <- holdDyn "" . leftmost $
                            [ "" <$ ev
                            , "is-active" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            tClass <- holdDyn "" . leftmost $
                            [ "" <$ ev
                            , "" <$ es
                            , "is-active" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "is-active" <$ em
                            , "" <$ ea
                            ]

            aClass <- holdDyn "" . leftmost $
                            [ "" <$ ev
                            , "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "is-active" <$ ea
                            ]

            return . leftmost $ [ev, es, et, em, ea]

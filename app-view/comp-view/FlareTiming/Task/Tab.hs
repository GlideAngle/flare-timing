module FlareTiming.Task.Tab (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabScore
    | TaskTabTask
    | TaskTabMap
    | TaskTabAbsent

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (s, _) <- elDynClass' "li" sClass $ el "a" (text "Scores")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Turnpoints")
            (m, _) <- elDynClass' "li" mClass $ el "a" (text "Map")
            (a, _) <- elDynClass' "li" aClass $ el "a" (text "Absentees")

            let es = (const TaskTabScore) <$> domEvent Click s
            let et = (const TaskTabTask) <$> domEvent Click t
            let em = (const TaskTabMap) <$> domEvent Click m
            let ea = (const TaskTabAbsent) <$> domEvent Click a

            sClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            tClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "is-active" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ et
                            , "is-active" <$ em
                            , "" <$ ea
                            ]

            aClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ et
                            , "" <$ em
                            , "is-active" <$ ea
                            ]

            return . leftmost $ [es, et, em, ea]

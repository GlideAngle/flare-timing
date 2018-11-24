module FlareTiming.Task.Tab (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabScore
    | TaskTabQuality
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
            (q, _) <- elDynClass' "li" qClass $ el "a" (text "Quality")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Turnpoints")
            (m, _) <- elDynClass' "li" mClass $ el "a" (text "Map")
            (a, _) <- elDynClass' "li" aClass $ el "a" (text "Absentees")

            let es = (const TaskTabScore) <$> domEvent Click s
            let eq = (const TaskTabQuality) <$> domEvent Click q
            let et = (const TaskTabTask) <$> domEvent Click t
            let em = (const TaskTabMap) <$> domEvent Click m
            let ea = (const TaskTabAbsent) <$> domEvent Click a

            sClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ es
                            , "" <$ eq
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            qClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "is-active" <$ eq
                            , "" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            tClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ eq
                            , "is-active" <$ et
                            , "" <$ em
                            , "" <$ ea
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ eq
                            , "" <$ et
                            , "is-active" <$ em
                            , "" <$ ea
                            ]

            aClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ eq
                            , "" <$ et
                            , "" <$ em
                            , "is-active" <$ ea
                            ]

            return . leftmost $ [es, eq, et, em, ea]

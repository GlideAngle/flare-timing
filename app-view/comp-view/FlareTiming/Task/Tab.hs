module FlareTiming.Task.Tab (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabScore
    | TaskTabTask
    | TaskTabMap

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (s, _) <- elDynClass' "li" sClass $ el "a" (text "Scores")
            (t, _) <- elDynClass' "li" tClass $ el "a" (text "Task")
            (m, _) <- elDynClass' "li" mClass $ el "a" (text "Map")

            let es = (const TaskTabScore) <$> domEvent Click s
            let et = (const TaskTabTask) <$> domEvent Click t
            let em = (const TaskTabMap) <$> domEvent Click m

            sClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ es
                            , "" <$ et
                            , "" <$ em
                            ]

            tClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "is-active" <$ et
                            , "" <$ em
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ es
                            , "" <$ et
                            , "is-active" <$ em
                            ]

            return . leftmost $ [es, et, em]

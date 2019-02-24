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
    | TaskTabArrive
    | TaskTabLead

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (geo, _) <- elDynClass' "li" gClass $ el "a" (text "Geo")
            (task, _) <- elDynClass' "li" tClass $ el "a" (text "Task")
            (map, _) <- elDynClass' "li" mClass $ el "a" (text "Map")
            (absent, _) <- elDynClass' "li" aClass $ el "a" (text "Pilots")
            (valid, _) <- elDynClass' "li" vClass $ el "a" (text "Valid")
            (score, _) <- elDynClass' "li" sClass $ el "a" (text "Score")
            (split, _) <- elDynClass' "li" pClass $ el "a" (text "Split")
            (arrive, _) <- elDynClass' "li" arClass $ el "a" (text "Arrive")
            (lead, _) <- elDynClass' "li" lClass $ el "a" (text "Lead")

            let eGeo = (const TaskTabGeo) <$> domEvent Click geo
            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eAbs = (const TaskTabAbsent) <$> domEvent Click absent
            let eValid = (const TaskTabValidity) <$> domEvent Click valid
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let eSplit = (const TaskTabSplit) <$> domEvent Click split
            let eArrive = (const TaskTabArrive) <$> domEvent Click arrive
            let eLead = (const TaskTabLead) <$> domEvent Click lead

            gClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            tClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            mClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            aClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            vClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "is-active" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            sClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "is-active" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            pClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "is-active" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            ]

            arClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "is-active" <$ eArrive
                            , "" <$ eLead
                            ]

            lClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "is-active" <$ eLead
                            ]

            return . leftmost $
                [ eGeo
                , eTask
                , eMap
                , eAbs
                , eValid
                , eScore
                , eSplit
                , eArrive
                , eLead
                ]

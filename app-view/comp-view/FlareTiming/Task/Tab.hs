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
    | TaskTabTime

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (geo, _) <- elDynClass' "li" geoClass $ el "a" (text "Geo")
            (task, _) <- elDynClass' "li" taskClass $ el "a" (text "Task")
            (map, _) <- elDynClass' "li" mapClass $ el "a" (text "Map")
            (absent, _) <- elDynClass' "li" absentClass $ el "a" (text "Pilots")
            (valid, _) <- elDynClass' "li" validClass $ el "a" (text "Valid")
            (score, _) <- elDynClass' "li" scoreClass $ el "a" (text "Score")

            (split, _) <- elDynClass' "li" splitClass . el "a" $ do
                            elClass "span" "legend-distance" $ text "▩"
                            elClass "span" "legend-arrival" $ text "▩"
                            elClass "span" "legend-leading" $ text "▩"
                            elClass "span" "legend-time" $ text "▩"
                            text "Split"

            (arrive, _) <- elDynClass' "li" arriveClass . el "a" $ do
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Arrive"

            (lead, _) <- elDynClass' "li" leadClass . el "a" $ do
                            elClass "span" "legend-leading" $ text "▩"
                            text "Lead"

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            let eGeo = (const TaskTabGeo) <$> domEvent Click geo
            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eAbs = (const TaskTabAbsent) <$> domEvent Click absent
            let eValid = (const TaskTabValidity) <$> domEvent Click valid
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let eSplit = (const TaskTabSplit) <$> domEvent Click split
            let eArrive = (const TaskTabArrive) <$> domEvent Click arrive
            let eLead = (const TaskTabLead) <$> domEvent Click lead
            let eTime = (const TaskTabTime) <$> domEvent Click time

            geoClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            taskClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            mapClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            absentClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            validClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "is-active" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            scoreClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "is-active" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            splitClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "is-active" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "is-active" <$ eArrive
                            , "" <$ eLead
                            , "" <$ eTime
                            ]

            leadClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "is-active" <$ eLead
                            , "" <$ eTime
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eArrive
                            , "" <$ eLead
                            , "is-active" <$ eTime
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
                , eTime
                ]

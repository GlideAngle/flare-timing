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
                            elClass "span" "legend-time" $ text "▩"
                            elClass "span" "legend-leading" $ text "▩"
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Split"

            (time, _) <- elDynClass' "li" timeClass . el "a" $ do
                            elClass "span" "legend-time" $ text "▩"
                            text "Time"

            (lead, _) <- elDynClass' "li" leadClass . el "a" $ do
                            elClass "span" "legend-leading" $ text "▩"
                            text "Lead"

            (arrive, _) <- elDynClass' "li" arriveClass . el "a" $ do
                            elClass "span" "legend-arrival" $ text "▩"
                            text "Arrive"

            let eGeo = (const TaskTabGeo) <$> domEvent Click geo
            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eAbs = (const TaskTabAbsent) <$> domEvent Click absent
            let eValid = (const TaskTabValidity) <$> domEvent Click valid
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let eSplit = (const TaskTabSplit) <$> domEvent Click split
            let eTime = (const TaskTabTime) <$> domEvent Click time
            let eLead = (const TaskTabLead) <$> domEvent Click lead
            let eArrive = (const TaskTabArrive) <$> domEvent Click arrive

            geoClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            taskClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            mapClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            absentClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            validClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "is-active" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            scoreClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "is-active" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            splitClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "is-active" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            timeClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "is-active" <$ eTime
                            , "" <$ eLead
                            , "" <$ eArrive
                            ]

            leadClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "is-active" <$ eLead
                            , "" <$ eArrive
                            ]

            arriveClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ eSplit
                            , "" <$ eTime
                            , "" <$ eLead
                            , "is-active" <$ eArrive
                            ]

            return . leftmost $
                [ eGeo
                , eTask
                , eMap
                , eAbs
                , eValid
                , eScore
                , eSplit
                , eTime
                , eLead
                , eArrive
                ]

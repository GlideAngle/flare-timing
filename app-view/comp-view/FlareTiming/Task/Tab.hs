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
    | TaskTabPlot

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
            (plot, _) <- elDynClass' "li" splitClass $ el "a" (text "Plot")

            let eGeo = (const TaskTabGeo) <$> domEvent Click geo
            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eAbs = (const TaskTabAbsent) <$> domEvent Click absent
            let eValid = (const TaskTabValidity) <$> domEvent Click valid
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let ePlot = (const TaskTabPlot) <$> domEvent Click plot

            geoClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ ePlot
                            ]

            taskClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ ePlot
                            ]

            mapClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eAbs
                            , "" <$ ePlot
                            ]

            absentClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eAbs
                            , "" <$ ePlot
                            ]

            validClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "is-active" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ ePlot
                            ]

            scoreClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "is-active" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "" <$ ePlot
                            ]

            splitClass <- holdDyn "" . leftmost $
                            [ "" <$ eGeo
                            , "" <$ eValid
                            , "" <$ eScore
                            , "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eAbs
                            , "is-active" <$ ePlot
                            ]

            return . leftmost $
                [ eGeo
                , eTask
                , eMap
                , eAbs
                , eValid
                , eScore
                , ePlot
                ]

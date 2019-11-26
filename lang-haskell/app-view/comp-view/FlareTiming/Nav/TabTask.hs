module FlareTiming.Nav.TabTask (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabTask
    | TaskTabMap
    | TaskTabScore
    | TaskTabPlot
    | TaskTabBasis

tabsTask
    :: MonadWidget t m
    => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (task, _) <- elDynClass' "li" taskClass $ el "a" (text "Task")
            (map, _) <- elDynClass' "li" mapClass $ el "a" (text "Map")
            (score, _) <- elDynClass' "li" scoreClass $ el "a" (text "Score")
            (plot, _) <- elDynClass' "li" plotClass $ el "a" (text "Plot")
            (basis, _) <- elDynClass' "li" basisClass $ el "a" (text "Basis")

            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let ePlot = (const TaskTabPlot) <$> domEvent Click plot
            let eBasis = (const TaskTabBasis) <$> domEvent Click basis

            taskClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            ]

            mapClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            ]

            scoreClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            ]

            plotClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "is-active" <$ ePlot
                            , "" <$ eBasis
                            ]

            basisClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "is-active" <$ eBasis
                            ]

            return . leftmost $
                [ eTask
                , eMap
                , eScore
                , ePlot
                , eBasis
                ]

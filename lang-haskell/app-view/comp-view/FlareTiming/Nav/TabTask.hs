module FlareTiming.Nav.TabTask (TaskTab(..), tabsTask) where

import Reflex
import Reflex.Dom

data TaskTab
    = TaskTabTask
    | TaskTabMap
    | TaskTabScore
    | TaskTabPlot
    | TaskTabBasis
    | TaskTabPenal
    | TaskTabVie

tabsTask :: MonadWidget t m => m (Event t TaskTab)
tabsTask =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (task, _) <- elDynClass' "li" taskClass $ el "a" (text "Task")
            (map, _) <- elDynClass' "li" mapClass $ el "a" (text "Map")
            (score, _) <- elDynClass' "li" scoreClass $ el "a" (text "Score")
            (plot, _) <- elDynClass' "li" plotClass $ el "a" (text "Plot")
            (basis, _) <- elDynClass' "li" basisClass $ el "a" (text "Basis")
            (penal, _) <- elDynClass' "li" penalClass $ el "a" (text "Penal")
            (vie, _) <- elDynClass' "li" vieClass $ el "a" (text "Vie")

            let eTask = (const TaskTabTask) <$> domEvent Click task
            let eMap = (const TaskTabMap) <$> domEvent Click map
            let eScore = (const TaskTabScore) <$> domEvent Click score
            let ePlot = (const TaskTabPlot) <$> domEvent Click plot
            let eBasis = (const TaskTabBasis) <$> domEvent Click basis
            let ePenal = (const TaskTabPenal) <$> domEvent Click penal
            let eVie = (const TaskTabVie) <$> domEvent Click vie

            taskClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            , "" <$ ePenal
                            , "" <$ eVie
                            ]

            mapClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "is-active" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            , "" <$ ePenal
                            , "" <$ eVie
                            ]

            scoreClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "is-active" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            , "" <$ ePenal
                            , "" <$ eVie
                            ]

            plotClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "is-active" <$ ePlot
                            , "" <$ eBasis
                            , "" <$ ePenal
                            , "" <$ eVie
                            ]

            basisClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "is-active" <$ eBasis
                            , "" <$ ePenal
                            , "" <$ eVie
                            ]

            penalClass <- holdDyn "" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            , "is-active" <$ ePenal
                            , "" <$ eVie
                            ]

            vieClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eTask
                            , "" <$ eMap
                            , "" <$ eScore
                            , "" <$ ePlot
                            , "" <$ eBasis
                            , "" <$ ePenal
                            , "is-active" <$ eVie
                            ]

            return . leftmost $
                [ eTask
                , eMap
                , eScore
                , ePlot
                , eBasis
                , ePenal
                , eVie
                ]

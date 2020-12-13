module FlareTiming.Nav.TabVie (VieTab(..), tabsVie) where

import Reflex
import Reflex.Dom

data VieTab
    = VieTabScore
    | VieTabScoreFs
    | VieTabPlotFs
    | VieTabScoreAs
    | VieTabPlotAs

tabsVie :: MonadWidget t m => m (Event t VieTab)
tabsVie =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (score, _) <- elDynClass' "li" scoreClass $ el "a" (text "3-Way (Ft,Fs,As)")
            (scoreFs, _) <- elDynClass' "li" scoreFsClass $ el "a" (text "Flight System (Fs)")
            (plotFs, _) <- elDynClass' "li" plotFsClass $ el "a" (text " Plot Fs")
            (scoreAs, _) <- elDynClass' "li" scoreAsClass $ el "a" (text "airScore (As)")
            (plotAs, _) <- elDynClass' "li" plotAsClass $ el "a" (text " Plot As")

            let eScore = (const VieTabScore) <$> domEvent Click score
            let eScoreFs = (const VieTabScoreFs) <$> domEvent Click scoreFs
            let ePlotFs = (const VieTabPlotFs) <$> domEvent Click plotFs
            let eScoreAs = (const VieTabScoreAs) <$> domEvent Click scoreAs
            let ePlotAs = (const VieTabPlotAs) <$> domEvent Click plotAs

            scoreClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eScore
                            , "" <$ eScoreFs
                            , "" <$ ePlotFs
                            , "" <$ eScoreAs
                            , "" <$ ePlotAs
                            ]

            scoreFsClass <- holdDyn "" . leftmost $
                            [ "" <$ eScore
                            , "is-active" <$ eScoreFs
                            , "" <$ ePlotFs
                            , "" <$ eScoreAs
                            , "" <$ ePlotAs
                            ]

            plotFsClass <- holdDyn "" . leftmost $
                            [ "" <$ eScore
                            , "" <$ eScoreFs
                            , "is-active" <$ ePlotFs
                            , "" <$ eScoreAs
                            , "" <$ ePlotAs
                            ]

            scoreAsClass <- holdDyn "" . leftmost $
                            [ "" <$ eScore
                            , "" <$ eScoreFs
                            , "" <$ ePlotFs
                            , "is-active" <$ eScoreAs
                            , "" <$ ePlotAs
                            ]

            plotAsClass <- holdDyn "" . leftmost $
                            [ "" <$ eScore
                            , "" <$ eScoreFs
                            , "" <$ ePlotFs
                            , "" <$ eScoreAs
                            , "is-active" <$ ePlotAs
                            ]

            return . leftmost $
                [ eScore
                , eScoreFs
                , ePlotFs
                , eScoreAs
                , ePlotAs
                ]

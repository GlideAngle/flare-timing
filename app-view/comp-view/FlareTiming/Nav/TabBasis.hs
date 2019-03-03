module FlareTiming.Nav.TabBasis (BasisTab(..), tabsBasis) where

import Reflex
import Reflex.Dom

data BasisTab
    = BasisTabAbsent
    | BasisTabValidity
    | BasisTabGeo

tabsBasis
    :: MonadWidget t m
    => m (Event t BasisTab)
tabsBasis =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (absent, _) <- elDynClass' "li" absentClass $ el "a" (text "Pilots")
            (valid, _) <- elDynClass' "li" validClass $ el "a" (text "Valid")
            (geo, _) <- elDynClass' "li" geoClass $ el "a" (text "Geo")

            let eAbs = (const BasisTabAbsent) <$> domEvent Click absent
            let eValid = (const BasisTabValidity) <$> domEvent Click valid
            let eGeo = (const BasisTabGeo) <$> domEvent Click geo

            absentClass <- holdDyn "is-active" . leftmost $
                            [ "" <$ eValid
                            , "is-active" <$ eAbs
                            , "" <$ eGeo
                            ]

            validClass <- holdDyn "" . leftmost $
                            [ "is-active" <$ eValid
                            , "" <$ eAbs
                            , "" <$ eGeo
                            ]

            geoClass <- holdDyn "" . leftmost $
                            [ "" <$ eValid
                            , "" <$ eAbs
                            , "is-active" <$ eGeo
                            ]

            return . leftmost $
                [ eAbs
                , eValid
                , eGeo
                ]

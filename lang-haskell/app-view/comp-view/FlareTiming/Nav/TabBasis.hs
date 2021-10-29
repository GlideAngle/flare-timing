module FlareTiming.Nav.TabBasis (BasisTab(..), tabsBasis) where

import Reflex
import Reflex.Dom

data BasisTab
    = BasisTabAbsent
    | BasisTabValidity
    | BasisTabGeo

tabsBasis :: MonadWidget t m => m (Event t BasisTab)
tabsBasis =
    elClass "div" "tabs" $
        el "ul" $ mdo
            (absent, _) <- elDynClass' "li" absentClass $ el "a" (text "Pilots")
            (valid, _) <- elDynClass' "li" validClass $ el "a" (text "Validity")
            (geo, _) <- elDynClass' "li" geoClass $ el "a" (text "Geo")

            let eAbs = (const BasisTabAbsent) <$> domEvent Click absent
            let eValid = (const BasisTabValidity) <$> domEvent Click valid
            let eGeo = (const BasisTabGeo) <$> domEvent Click geo

            absentClass <- holdDyn "is-active" . leftmost $
                            [ "is-active" <$ eAbs
                            , "" <$ eValid
                            , "" <$ eGeo
                            ]

            validClass <- holdDyn "" . leftmost $
                            [ "" <$ eAbs
                            , "is-active" <$ eValid
                            , "" <$ eGeo
                            ]

            geoClass <- holdDyn "" . leftmost $
                            [ "" <$ eAbs
                            , "" <$ eValid
                            , "is-active" <$ eGeo
                            ]

            return . leftmost $
                [ eAbs
                , eValid
                , eGeo
                ]

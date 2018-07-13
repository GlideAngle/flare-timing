{-# LANGUAGE OverloadedStrings #-}

module FlareTiming.Footer (footer) where

import Prelude hiding (map)
import Reflex.Dom (MonadWidget, (=:), elAttr, elClass, el, text)
import Data.Map (union)

{-# ANN module "HLint: ignore" #-}

attribution :: MonadWidget t m => m () 
attribution = do
    el "p" $ do
        el "small" $ do
            text "Map data: (c) "
            elAttr "a" (union ("href" =: "http://www.openstreetmap.org/copyright")
                              ("target" =: "_blank")) $ do
                text "OpenStreetMap"

            text ", "
            elAttr "a" (union ("href" =: "http://viewfinderpanoramas.org")
                              ("target" =: "_blank")) $ do
                text "SRTM"

            text " | Map style: (c) "
            elAttr "a" (union ("href" =: "https://opentopomap.org")
                              ("target" =: "_blank")) $ do
                text "OpenTopoMap"

            text " ("
            elAttr "a" (union ("href" =: "https://creativecommons.org/licenses/by-sa/3.0/")
                              ("target" =: "_blank")) $ do
                text "CC-BY-SA"

            text ")"


footer :: MonadWidget t m => m ()
footer =
    elClass "footer" "footer" $ do
        elClass "div" "container" $ do
            elClass "div" "content has-text-centered" $ do
                attribution

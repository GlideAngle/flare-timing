module FlareTiming.Footer (footer) where

import Reflex.Dom

attribution :: MonadWidget t m => m () 
attribution =
    elClass "div" "is-size-7" $ do
    elAttr "strong" ("title" =: "app-view-0.3 2018-12-19T10:27") $ text "Flare Timing"
    text " by "
    elAttr
        "a"
        ("href" =: "http://www.blockscope.com" <> "target" =: "_blank")
        $ text "Block Scope"

    el "br" $ return ()
    text "Map data © "

    elAttr
        "a"
        ("href" =: "http://www.openstreetmap.org/copyright" <> "target" =: "_blank")
        $ text "OpenStreetMap"

    text ", "

    elAttr
        "a"
        ("href" =: "http://viewfinderpanoramas.org" <> "target" =: "_blank")
        $ text "SRTM"

    el "br" $ return ()
    text "Map style © "

    elAttr
        "a" ("href" =: "https://opentopomap.org" <> "target" =: "_blank")
        $ text "OpenTopoMap"

    text " ("

    elAttr
        "a"
        ("href" =: "https://creativecommons.org/licenses/by-sa/3.0/" <> "target" =: "_blank")
        $ text "CC-BY-SA"

    text ")"


footer :: MonadWidget t m => m ()
footer =
    elClass "footer" "footer" $ do
        elClass "div" "container" $ do
            elClass "div" "content" $ do
                attribution

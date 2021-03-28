module FlareTiming.Footer (footer) where

import Reflex.Dom

attribution :: MonadWidget t m => m ()
attribution =
    elClass "div" "is-size-7" $ do
    elAttr "strong" ("title" =: "app-view-0.30 2021-03-28T07:11") $ text "Flare Timing"
    text " by "
    elAttr
        "a"
        ("href" =: "http://www.blockscope.com" <> "target" =: "_blank")
        $ text "Block Scope"

    el "br" $ return ()

    text "("
    elAttr
        "a"
        ("href" =: "http://www.flaretiming.com/about.html" <> "target" =: "_blank")
        $ text "About"

    text ", "
    elAttr
        "a"
        ("href" =: "http://www.flaretiming.com/disclaim.html" <> "target" =: "_blank")
        $ text "Disclaimer"

    text ", "
    elAttr
        "a"
        ("href" =: "http://www.flaretiming.com/blog.html" <> "target" =: "_blank")
        $ text "Blog"

    text ")"
    el "br" $ return ()
    el "br" $ return ()
    text "Map data © "

    elAttr
        "a"
        ("href" =: "http://www.openstreetmap.org/copyright" <> "target" =: "_blank")
        $ text "OpenStreetMap"

    text " contributors"

footer :: MonadWidget t m => m ()
footer =
    elClass "footer" "footer" $ do
        elClass "div" "container" $ do
            elClass "div" "content" $ do
                attribution

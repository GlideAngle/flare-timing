{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module FlareTiming.Comp (comps) where

import Prelude hiding (map)
import Data.Maybe (isJust)
import Reflex.Dom
    ( MonadWidget, Event, Dynamic, XhrRequest(..)
    , (=:)
    , def
    , holdDyn
    , sample
    , current
    , widgetHold
    , elAttr
    , elClass
    , el
    , text
    , dynText
    , simpleList
    , getPostBuild
    , fmapMaybe
    , performRequestAsync
    , decodeXhrResponse
    , leftmost
    )
import qualified Data.Text as T (Text, pack)
import Data.Map (union)
import Data.List (intercalate)

import Data.Flight.Types (Comp(..))
import FlareTiming.NavBar (navbar)
import FlareTiming.Footer (footer)

loading :: MonadWidget t m => m ()
loading = do
    el "li" $ do
        text "Comps will be shown here"

getName :: Comp -> String
getName Comp{..} = name

comp :: forall t (m :: * -> *).
        MonadWidget t m =>
        Dynamic t (Int, Comp) -> m ()
comp ix = do
    i :: String <- sample $ current $ fmap (show . fst) ix 
    let x :: Dynamic t Comp = fmap snd ix
    let title = fmap (T.pack . (\Comp{..} -> name)) x
    let subtitle =
            fmap (T.pack . (\Comp{..} ->
                mconcat [ location
                        , ", from "
                        , from
                        , " to "
                        , to
                        ])) x

    y :: Comp <- sample $ current x

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title
                    elClass "p" "title is-5" $ do
                        dynText subtitle
                
comps :: MonadWidget t m => m ()
comps = do
    pb :: Event t () <- getPostBuild
    navbar
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        el "ul" $ do widgetHold loading $ fmap getComps pb
        elClass "div" "spacer" $ return ()
        footer

    return ()

getComps :: MonadWidget t m => () -> m ()
getComps () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/comps"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]
        
    let es :: Event t [Comp] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Comp] <- holdDyn [] es
    let ys :: Dynamic t [(Int, Comp)] = fmap (zip [1 .. ]) xs

    elAttr "div" (union ("class" =: "tile is-ancestor")
                        ("style" =: "flex-wrap: wrap;")) $ do
        simpleList ys comp

    return ()


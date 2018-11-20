{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module FlareTiming.Comp (comps) where

import Prelude hiding (map)
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
    , ffor
    , dyn
    )
import qualified Data.Text as T (pack)
import Data.Map (union)
import Data.List (find)

import Data.Flight.Types (Comp(..), Nominal(..))
import FlareTiming.NavBar (navbar)
import FlareTiming.Footer (footer)

loading :: MonadWidget t m => m ()
loading = do
    el "li" $ do
        text "Comps will be shown here"

comp
    :: forall t (m :: * -> *). MonadWidget t m
    => Dynamic t [(Int, Nominal)]
    -> Dynamic t (Int, Comp)
    -> m ()
comp ns cs = do
    let i :: Dynamic t Int = fmap fst cs 
    ii :: Int <- sample $ current i
    let c :: Dynamic t Comp = fmap snd cs 
    let n :: Dynamic t (Maybe (Int, Nominal)) = fmap (find (\(iN, _) -> iN == ii)) ns
    let title = fmap (T.pack . (\Comp{..} -> compName)) c
    let subtitle =
            fmap (T.pack . (\Comp{..} ->
                mconcat [ from
                        , " to "
                        , to
                        , ", "
                        , location
                        ])) c

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title
                    elClass "p" "title is-5" $ do
                        dynText subtitle
                elClass "div" "example" $ do
                    nominal n

nominal
    :: forall t (m :: * -> *).  MonadWidget t m
    => Dynamic t (Maybe (Int, Nominal))
    -> m ()
nominal n = do
    _ <- dyn $ ffor n (\x ->
        case x of
            Nothing -> text "Loading nominals ..."
            (Just (_, Nominal{..})) -> do
                elClass "div" "field is-grouped is-grouped-multiline" $ do
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal distance"
                            elClass "span" "tag is-info" $ do
                                text $ T.pack distance
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal time"
                            elClass "span" "tag is-success" $ do
                                text $ T.pack time
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "nominal goal"
                            elClass "span" "tag is-primary"
                                $ text (T.pack . show $ goal))

    return ()

comps :: MonadWidget t m => m ()
comps = do
    pb :: Event t () <- getPostBuild
    navbar
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        _ <- el "ul" $ do
            widgetHold loading $ fmap viewComps pb
        elClass "div" "spacer" $ return ()
        footer

    return ()

getComps
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [(Int, Comp)])
getComps () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/comps"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Comp] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Comp] <- holdDyn [] es
    let ys :: Dynamic t [(Int, Comp)] = fmap (zip [1 .. ]) xs
    return ys

getNominals
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [(Int, Nominal)])
getNominals () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/nominals"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Nominal] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Nominal] <- holdDyn [] es
    let ys :: Dynamic t [(Int, Nominal)] = fmap (zip [1 .. ]) xs
    return ys

viewComps :: MonadWidget t m => () -> m ()
viewComps () = do
    cs <- getComps ()
    ns <- getNominals ()

    _ <-
        elAttr
            "div"
            (union
                ("class" =: "tile is-ancestor")
                ("style" =: "flex-wrap: wrap;")
            ) $ do
        simpleList cs (comp ns)

    return ()

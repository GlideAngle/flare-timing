{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module FlareTiming.View (tasks) where

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

import FlareTiming.Task
    ( Task(..)
    , Turnpoint(..)
    , Latitude(..)
    , Longitude(..)
    , Name
    , Radius
    , SpeedSection
    , fromSci
    , toSci
    , showRadius
    )
import FlareTiming.Map (map)
import FlareTiming.NavBar (navbar)
import FlareTiming.Footer (footer)

loading :: MonadWidget t m => m ()
loading = do
    el "li" $ do
        text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = do
    pb :: Event t () <- getPostBuild
    navbar
    elClass "div" "spacer" $ return ()
    elClass "div" "container" $ do
        el "ul" $ do widgetHold loading $ fmap getTasks pb
        elClass "div" "spacer" $ return ()
        footer

    return ()

liTurnpointRadius :: forall t (m :: * -> *).
             MonadWidget t m =>
             Dynamic t Turnpoint -> m ()
liTurnpointRadius x = do
    let dyTp :: Dynamic t T.Text =
            fmap (\(Turnpoint name _ _ radius) ->
                T.pack $ name ++ " " ++ showRadius radius) x

    el "li" $ do
        dynText dyTp

task :: forall t (m :: * -> *).
        MonadWidget t m =>
        Dynamic t Task -> m ()
task x = do
    let dyName :: Dynamic t T.Text =
            fmap (\(Task name _ _) -> T.pack name) x

    let dyTurnpoints :: Dynamic t [Turnpoint] =
            fmap (\(Task _ ss tps) -> speedSectionOnly ss tps) x

    y :: Task <- sample $ current x

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title" $ do
                    dynText dyName
                    map y
                el "ul" $ do
                    simpleList dyTurnpoints liTurnpointRadius
                    return ()
    where
        speedSectionOnly :: SpeedSection -> [Turnpoint] -> [Turnpoint]
        speedSectionOnly Nothing xs =
            xs
        speedSectionOnly (Just (start, end)) xs =
            take (end' - start' + 1) $ drop (start' - 1) xs
            where
                start' = fromInteger start
                end' = fromInteger end

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]
        
    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Task] <- holdDyn [] es

    elAttr "div" (union ("class" =: "tile is-ancestor")
                        ("style" =: "flex-wrap: wrap;")) $ do
        simpleList xs task

    return ()


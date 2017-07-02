{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Task (tasks) where

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

import FlareTiming.WireTypes
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
import qualified FlareTiming.Turnpoint as TP (turnpoint, turnpointRadius, getName)

loading :: MonadWidget t m => m ()
loading = do
    el "li" $ do
        text "Tasks will be shown here"

getName :: Task -> String
getName (Task name _ _) = name

getSpeedSection :: Task -> [Turnpoint]
getSpeedSection (Task _ ss tps) =
    speedSectionOnly ss tps
    where
        speedSectionOnly :: SpeedSection -> [Turnpoint] -> [Turnpoint]
        speedSectionOnly Nothing xs =
            xs
        speedSectionOnly (Just (start, end)) xs =
            take (end' - start' + 1) $ drop (start' - 1) xs
            where
                start' = fromInteger start
                end' = fromInteger end

hyphenate :: [Turnpoint] -> String
hyphenate xs =
    intercalate " - " $ fmap TP.getName xs

task :: forall t (m :: * -> *).
        MonadWidget t m =>
        Dynamic t (Int, Task) -> m ()
task ix = do
    i :: String <- sample $ current $ fmap (show . fst) ix 
    let x :: Dynamic t Task = fmap snd ix
    let title = fmap (T.pack . getName) x
    let xs = fmap getSpeedSection x
    let subtitle = fmap (T.pack . (\s -> "#" ++ i ++ " - " ++ s) . hyphenate) xs

    y :: Task <- sample $ current x

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                map y
                elClass "p" "" $ do
                    el "a" $ do
                        dynText subtitle
                
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

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]
        
    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Task] <- holdDyn [] es
    let ys :: Dynamic t [(Int, Task)] = fmap (zip [1 .. ]) xs

    elAttr "div" (union ("class" =: "tile is-ancestor")
                        ("style" =: "flex-wrap: wrap;")) $ do
        simpleList ys task

    return ()


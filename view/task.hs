{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

import Control.Monad
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid((<>))
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)

type Name = String
newtype Latitude = Latitude Rational deriving (Eq, Show)
newtype Longitude = Longitude Rational deriving (Eq, Show)
type Radius = Integer
type SpeedSection = Maybe (Integer, Integer)

data Task = Task Name SpeedSection [Turnpoint] deriving (Eq, Show, Generic)
data Turnpoint = Turnpoint Name Latitude Longitude Radius deriving (Eq, Show, Generic)

instance ToJSON Turnpoint
instance FromJSON Turnpoint

instance ToJSON Task
instance FromJSON Task

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci  :: Rational -> Scientific
toSci x =
    case fromRationalRepetend Nothing x of
        Left (s, _) -> s
        Right (s, _) -> s

instance ToJSON Latitude where
    toJSON (Latitude x) = Number $ toSci x

instance FromJSON Latitude where
    parseJSON x@(Number _) = Latitude . fromSci <$> parseJSON x
    parseJSON _ = empty

instance ToJSON Longitude where
    toJSON (Longitude x) = Number $ toSci x

instance FromJSON Longitude where
    parseJSON x@(Number _) = Longitude . fromSci <$> parseJSON x
    parseJSON _ = empty

main :: IO ()
main = mainWidget tasks

loading :: MonadWidget t m => m ()
loading = do
    el "li" $ do
        text "Tasks will be shown here"

tasks :: MonadWidget t m => m ()
tasks = el "div" $ do
    evGet <- button "Get Tasks"
    el "ul" $ do
        widgetHold loading $ fmap getTasks evGet
    return ()

turnpoint :: forall t (m :: * -> *).
             MonadWidget t m =>
             Dynamic t Turnpoint -> m ()
turnpoint x = do
    let dyName :: Dynamic t T.Text =
            fmap (\(Turnpoint name _ _ _) -> T.pack name) x

    el "li" $ do
        dynText dyName

task :: forall t (m :: * -> *).
        MonadWidget t m =>
        Dynamic t Task -> m ()
task x = do
    let dyName :: Dynamic t T.Text =
            fmap (\(Task name _ _) -> T.pack name) x

    let dySpeedSection :: Dynamic t SpeedSection =
            fmap (\(Task _ ss _) -> ss) x

    let dyTurnpoints :: Dynamic t [Turnpoint] =
            fmap (\(Task _ _ tps) -> tps) x

    el "li" $ do
        dynText dyName
        el "ul" $ do
            el "li" $ do display dySpeedSection
            simpleList dyTurnpoints turnpoint
            return ()

getTasks :: MonadWidget t m => () -> m ()
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rec rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]
        
    let evTasks :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    dyTasks :: Dynamic t [Task] <- holdDyn [] evTasks
    simpleList dyTasks task 
    return ()

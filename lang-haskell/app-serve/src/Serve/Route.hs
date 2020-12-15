module Serve.Route
    ( getTaskRouteSphericalEdge
    , getTaskRouteEllipsoidEdge
    , getTaskRouteProjectedSphericalEdge
    , getTaskRouteProjectedEllipsoidEdge
    , getTaskRouteProjectedPlanarEdge
    , getTaskRouteLengths
    ) where

import Data.Maybe (isNothing, catMaybes)
import Data.UnitsOfMeasure (u)
import Servant (throwError)
import Control.Monad (join)
import Control.Monad.Reader (asks)

import Flight.Units ()
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Comp (Comp(..), CompSettings(..))
import Flight.Route
    ( OptimalRoute(..), TaskTrack(..), TrackLine(..)
    , ProjectedTrackLine(..), PlanarTrackLine(..)
    )
import Flight.Distance (QTaskDistance)
import Serve.Config (AppT(..), Config(..))
import Serve.Error (errTaskStep, errTaskBounds, errTaskLengths)

getTaskRouteSphericalEdge :: Int -> AppT k IO (OptimalRoute (Maybe TrackLine))
getTaskRouteSphericalEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just TaskTrack{sphericalEdgeToEdge = x} : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteEllipsoidEdge :: Int -> AppT k IO (OptimalRoute (Maybe TrackLine))
getTaskRouteEllipsoidEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just TaskTrack{ellipsoidEdgeToEdge = x} : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedSphericalEdge :: Int -> AppT k IO TrackLine
getTaskRouteProjectedSphericalEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { spherical = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedEllipsoidEdge :: Int -> AppT k IO TrackLine
getTaskRouteProjectedEllipsoidEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { ellipsoid = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedPlanarEdge :: Int -> AppT k IO PlanarTrackLine
getTaskRouteProjectedPlanarEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { planar = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteLengths :: AppT k IO [QTaskDistance Double [u| m |]]
getTaskRouteLengths = do
    Comp{earth = e, earthMath = m} <- comp <$> asks compSettings
    xs' <- asks routing
    case xs' of
        Just xs -> do
            let ds = [join $ fmap (getRouteLength e m) x | x <- xs]

            if any isNothing ds
               then throwError errTaskLengths
               else return $ catMaybes ds

        _ -> throwError errTaskLengths

ellipsoidRouteLength :: TaskTrack -> Maybe (QTaskDistance Double [u| m |])
ellipsoidRouteLength
    TaskTrack
        { ellipsoidEdgeToEdge =
            OptimalRoute{taskRoute = Just TrackLine{distance = d}}} = Just d
ellipsoidRouteLength
    TaskTrack
        { ellipsoidEdgeToEdge =
            OptimalRoute{taskRoute = Nothing}} = Nothing

flatRouteLength :: TaskTrack -> Maybe (QTaskDistance Double [u| m |])
flatRouteLength
    TaskTrack
        { projection =
            OptimalRoute
                { taskRoute =
                    Just
                        ProjectedTrackLine
                            {spherical =
                                TrackLine
                                    {distance = d}}}} = Just d
flatRouteLength
    TaskTrack
        { projection = OptimalRoute{taskRoute = Nothing} } = Nothing

getRouteLength
    :: EarthModel Double
    -> EarthMath
    -> TaskTrack
    -> Maybe (QTaskDistance Double [u| m |])
getRouteLength (EarthAsSphere _) Haversines
    TaskTrack
        { sphericalEdgeToEdge =
            OptimalRoute
                { taskRoute =
                    Just
                        TrackLine
                            {distance = d}}} = Just d
getRouteLength (EarthAsEllipsoid _) Vincenty track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) AndoyerLambert track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) ForsytheAndoyerLambert track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) FsAndoyer track = ellipsoidRouteLength track
getRouteLength (EarthAsFlat _) Pythagorus track = flatRouteLength track
getRouteLength _ _ _ = Nothing


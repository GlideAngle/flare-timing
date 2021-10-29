module Serve.Alt
    ( getAltTaskScore
    , getAltTaskValidityWorking
    , getAltTaskRouteSphere
    , getAltTaskRouteEllipse
    , getAltTaskLanding
    , getAltTaskArrival
    ) where

import Data.List (zip4)
import Servant (throwError)
import Control.Monad (join)
import Control.Monad.Reader (asks)

import Flight.Units ()
import Flight.Track.Land (TaskLanding(..), taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Place (rankByAirScoreTotal)
import qualified Flight.Track.Mask as Mask (CompMaskingArrival(..))
import qualified Flight.Track.Point as Alt (AlternativePointing(..), AltBreakdown(..))
import qualified "flight-gap-valid" Flight.Score as Vw (ValidityWorking(..))
import Flight.Comp (AltDot(AltFs, AltAs), IxTask(..), Pilot(..))
import Flight.Route (TrackLine(..), GeoLines(..))
import Serve.Validity (nullValidityWorking)
import Serve.Config (AppT(..), Config(..))
import Serve.Error (errTaskStep, errTaskBounds)

getAltTaskValidityWorking :: Int -> AppT k IO (Maybe Vw.ValidityWorking)
getAltTaskValidityWorking ii = do
    ls' <- fmap Alt.validityWorkingLaunch <$> asks altFsScore
    ts' <- fmap Alt.validityWorkingTime <$> asks altFsScore
    ds' <- fmap Alt.validityWorkingDistance <$> asks altFsScore
    ss' <- fmap Alt.validityWorkingStop <$> asks altFsScore
    case (ls', ts', ds', ss') of
        (Just ls, Just ts, Just ds, Just ss) ->
            case drop (ii - 1) $ zip4 ls ts ds ss of
                (lv, tv, dv, sv) : _ -> return $ do
                    lv' <- lv
                    tv' <- tv
                    dv' <- dv
                    return $
                        nullValidityWorking
                            { Vw.launch = lv'
                            , Vw.time = tv'
                            , Vw.distance = dv'
                            , Vw.stop = sv
                            }

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "gap-point" ii

getAltTaskScore :: AltDot -> Int -> AppT k IO [(Pilot, Alt.AltBreakdown)]

getAltTaskScore AltFs ii = do
    xs' <- fmap Alt.score <$> asks altFsScore
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-score" ii

getAltTaskScore AltAs ii = do
    xs' <- fmap Alt.score <$> asks altAsScore
    let asSorted = (fmap . fmap) rankByAirScoreTotal xs'
    case asSorted of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "as-score" ii

getAltTaskRouteSphere :: Int -> AppT k IO (Maybe TrackLine)
getAltTaskRouteSphere ii = do
    xs' <- asks altFsRoute
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return $ sphere x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-route" ii

getAltTaskRouteEllipse :: Int -> AppT k IO (Maybe TrackLine)
getAltTaskRouteEllipse ii = do
    xs' <- asks altFsRoute
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return $ ellipse x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-route" ii

getAltTaskLanding :: Int -> AppT k IO (Maybe TaskLanding)
getAltTaskLanding ii = do
    x <- asks altFsLandout
    return . join $ taskLanding (IxTask ii) <$> x

getAltTaskArrival :: Int -> AppT k IO [(Pilot, TrackArrival)]
getAltTaskArrival ii = do
    xs' <- fmap Mask.arrivalRank <$> asks maskingArrival
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii


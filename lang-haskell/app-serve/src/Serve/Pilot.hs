module Serve.Pilot
    ( getTaskPilotAbs
    , getTaskPilotDnf
    , getTaskPilotDfNoTrack
    , getTaskPilotNyp
    , getTaskPilotDf
    , getTaskPilotTrack
    , getTaskPilotArea
    , getTaskPilotTrackFlyingSection
    , getTaskPilotTrackScoredSection
    , getTaskPilotCross
    , getTaskPilotTag
    , getPilots
    , getPilotsStatus
    , getScores
    ) where

import Data.List ((\\), nub, sort, sortOn, find)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Servant (throwError)
import Control.Exception.Safe (catchIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Numeric.Sampling

import Flight.Units ()
import qualified Flight.Track.Cross as Cg
    (CompFlying(..), CompCrossing(..), PilotTrackCross(..))
import qualified Flight.Track.Tag as Tg (CompTagging(..), PilotTrackTag(..))
import qualified Flight.Track.Stop as Sp (CompFraming(..))
import Flight.Track.Cross (TrackFlyingSection(..), ZoneTag(..), TrackCross(..))
import Flight.Track.Stop (StopFraming(..), TrackScoredSection(..))
import Flight.Track.Lead (CompLeading(..))
import Flight.Track.Time (TickRow(..))
import Flight.Track.Mask (CompMaskingLead(..))
import Flight.Track.Point (CompPointing(..), Velocity(..), Breakdown(..))
import "flight-gap-allot" Flight.Score (PilotVelocity(..))
import Flight.Scribe (readPilotDiscardFurther)
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , PilotId(..)
    , PilotGroup(..)
    , PilotTaskStatus(..)
    , PilotTrackLogFile(..)
    , DfNoTrackPilot(..)
    , DfNoTrack(..)
    , Nyp(..)
    , CompTaskSettings(..)
    )
import Flight.Mask (checkTracks)
import Data.Ratio.Rounding (dpRound)
import Serve.Track (RawLatLngTrack(..), crossToTrack, tagToTrack)
import Serve.Area (RawLeadingArea(..))
import Serve.Config (AppT(..), Config(..))
import Serve.Error (errTaskBounds, errPilotNotFound, errPilotTrackNotFound)

roundVelocity
    :: PilotVelocity (Quantity Double [u| km / h |])
    -> PilotVelocity (Quantity Double [u| km / h |])
roundVelocity (PilotVelocity (MkQuantity d)) =
    PilotVelocity . MkQuantity . fromRational . (dpRound 1) . toRational $ d

roundVelocity' :: Breakdown -> Breakdown
roundVelocity' b@Breakdown{velocity = Just v@Velocity{gsVelocity = (Just x)}} =
    b{velocity = Just v{gsVelocity = Just . roundVelocity $ x}}
roundVelocity' b = b

getScores :: CompPointing -> [[(Pilot, Breakdown)]]
getScores = ((fmap . fmap . fmap) roundVelocity') . score

getScoresDf :: CompPointing -> [[(Pilot, Breakdown)]]
getScoresDf = ((fmap . fmap . fmap) roundVelocity') . scoreDf

distinctPilots :: [[PilotTrackLogFile]] -> [Pilot]
distinctPilots pss =
    let pilot (PilotTrackLogFile p _) = p
    in sort . nub .concat $ (fmap . fmap) pilot pss

getPilots :: CompTaskSettings k -> [Pilot]
getPilots = distinctPilots . pilots

getTaskPilotAbs :: Int -> AppT k IO [Pilot]
getTaskPilotAbs ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{..} : _ -> return absent
        _ -> throwError $ errTaskBounds ii

getTaskPilotDnf :: Int -> AppT k IO [Pilot]
getTaskPilotDnf ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{..} : _ -> return dnf
        _ -> throwError $ errTaskBounds ii

getTaskPilotDfNoTrack :: Int -> AppT k IO [DfNoTrackPilot]
getTaskPilotDfNoTrack ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{didFlyNoTracklog = DfNoTrack zs} : _ -> return zs
        _ -> throwError $ errTaskBounds ii

nyp
    :: [Pilot]
    -> PilotGroup
    -> [(Pilot, b)]
    -> Nyp
nyp ps PilotGroup{absent = xs, dnf = ys, didFlyNoTracklog = DfNoTrack zs} cs =
    let (cs', _) = unzip cs in
    Nyp $ ps \\ (xs ++ ys ++ (pilot <$> zs) ++ cs')

status
    :: PilotGroup
    -> [Pilot] -- ^ Pilots that DF this task
    -> Pilot -- ^ Get the status for this pilot
    -> PilotTaskStatus
status PilotGroup{absent = xs, dnf = ys, didFlyNoTracklog = DfNoTrack zs} cs p =
    if | p `elem` xs -> ABS
       | p `elem` ys -> DNF
       | p `elem` cs -> DF
       | p `elem` (pilot <$> zs) -> DFNoTrack
       | otherwise -> NYP

getTaskPilotNyp :: Int -> AppT k IO [Pilot]
getTaskPilotNyp ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    pgs <- pilotGroups <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing
    case css' of
        Just css ->
            case (drop jj pgs, drop jj css) of
                (pg : _, cs : _) ->
                    let Nyp ps' = nyp ps pg cs in return ps'
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getTaskPilotDf :: Int -> AppT k IO [Pilot]
getTaskPilotDf ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing
    case css' of
        Just css ->
            case drop jj css of
                cs : _ -> return . fst . unzip $ cs
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getPilotsStatus :: AppT k IO [(Pilot, [PilotTaskStatus])]
getPilotsStatus = do
    ps <- getPilots <$> asks compSettings
    pgs <- pilotGroups <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing

    let fs =
            case css' of
              Just css ->
                    [ status pg $ fst <$> cs
                    | pg <- pgs
                    | cs <- css
                    ]

              _ -> repeat $ const NYP

    return $ [(p,) $ ($ p) <$> fs | p <- ps]

getTaskPilotTrack :: Int -> String -> AppT k IO RawLatLngTrack
getTaskPilotTrack ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    inFiles <- asks inputFiles
    ps <- getPilots <$> asks compSettings
    let p = find (\(Pilot (pid, _)) -> pid == pilot) ps

    case p of
        Nothing -> throwError $ errPilotNotFound pilot
        Just p' -> do
            t <- checkTracks (const $ const id) inFiles [ix] [p']
            case take 1 $ drop jj t of
                [t' : _] ->
                    case t' of
                        Right (_, mf) -> return $ RawLatLngTrack mf
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskPilotArea :: Int -> String -> AppT k IO RawLeadingArea
getTaskPilotArea ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    (cf, _) <- asks inputFiles
    ml <- asks maskingLead
    dl <- asks discardingLead2
    ps <- getPilots <$> asks compSettings
    let p = find (\(Pilot (pid, _)) -> pid == pilot) ps

    case (p, ml, dl) of
        (Nothing, _, _) -> throwError $ errPilotNotFound pilot
        (_, Nothing, _) -> throwError $ errPilotNotFound pilot
        (_, _, Nothing) -> throwError $ errPilotNotFound pilot
        (Just p'
            , Just CompMaskingLead{raceTime = rt, raceDistance = rd}
            , Just CompLeading{areas = sq}) -> do

            xs <-
                liftIO $ catchIO
                    (Just <$> readPilotDiscardFurther cf ix p')
                    (const $ return Nothing)

            case (xs, take 1 $ drop jj rt, take 1 $ drop jj rd, take 1 $ drop jj sq) of
                (Nothing, _, _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, [], _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, _, [], _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, _, _, []) -> throwError $ errPilotTrackNotFound ix pilot
                (Just [], _, _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just (x0 : xs1toN), t : _, d : _, areas : _) ->
                    case find (\(p'', _) -> p'' == p') areas of
                        Nothing -> throwError $ errPilotTrackNotFound ix pilot
                        Just (_, a) ->
                            case reverse xs1toN of
                                [] -> return $ RawLeadingArea t d (x0 : xs1toN) a
                                (xsN : xs2toM) -> do
                                    ys <- liftIO $ resampleIO 500 xs2toM
                                    let zs = x0 : (sortOn fixIdx ys) ++ [xsN]
                                    return $ RawLeadingArea t d (nub zs) a

getTaskPilotTrackFlyingSection :: Int -> String -> AppT k IO TrackFlyingSection
getTaskPilotTrackFlyingSection ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Cg.flying <$> asks flying
    let isPilot (Pilot (pid, _)) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find (isPilot . fst) fs of
                        Just (_, Just y) -> return y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskPilotTrackScoredSection :: Int -> String -> AppT k IO TrackScoredSection
getTaskPilotTrackScoredSection ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Sp.stopFlying <$> asks framing
    let isPilot (Pilot (pid, _)) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find (isPilot . fst) fs of
                        Just (_, StopFraming{stopScored = Just y}) -> return y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskPilotCross :: Int -> String -> AppT k IO (Maybe TrackCross)
getTaskPilotCross ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Cg.crossing <$> asks crossing
    let isPilot (Cg.PilotTrackCross (Pilot (pid, _)) _) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find isPilot fs of
                        Just y -> return $ crossToTrack y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskPilotTag :: Int -> String -> AppT k IO [Maybe ZoneTag]
getTaskPilotTag ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Tg.tagging <$> asks tagging
    let isPilot (Tg.PilotTrackTag (Pilot (pid, _)) _) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find isPilot fs of
                        Just y -> return $ tagToTrack y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot


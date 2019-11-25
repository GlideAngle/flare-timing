{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Mask.Group.Double () where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.These
import Data.List (nub)
import Data.List.Split (split, whenElt, keepDelimsL, keepDelimsR)
import qualified Data.Map as Map
import Control.Monad (join)

import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Zone.Raw (Give)
import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Units ()
import Flight.Kml (MarkedFixes(..), fixToUtc)
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (Fix(..), ZoneCross(..), ZoneTag(..))
import Flight.Track.Time (LegIdx(..))
import Flight.Comp (Task(..), TimePass)
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Span.Sliver (GeoSliver(..))
import Flight.Span.Double ()
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    )
import Flight.Mask.Tag (GeoTag(..))
import Flight.Mask.Tag.Double ()
import Flight.Mask.Group (GroupLeg(..), GeoLeg(..))

instance GeoTag Double a => GeoLeg Double a where
    groupByLeg
        :: (FlyClipping UTCTime MarkedFixes, Trig Double a)
        => Earth Double
        -> Maybe Give
        -> SampleParams Double
        -> [TimePass]
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> [(Maybe GroupLeg, MarkedFixes)]
    groupByLeg e give sp timechecks task@Task{zones} flyCut =
        [
            let g =
                    case (nthR, zerothL) of
                    (Nothing, Nothing) ->
                        Nothing
                    (Nothing, Just (_, Nothing)) ->
                        Nothing
                    (Just (_, Nothing), Nothing) ->
                        Nothing
                    (Just (_, Nothing), Just (_, Nothing)) ->
                        Nothing
                    (Just (tR, Just iR), Just (tL, Just iL)) ->
                        Just
                            GroupLeg
                                { groupLeg = These iL iR
                                , groupTime = These tL tR
                                }
                    (Nothing, Just (tL, Just iL)) ->
                        Just
                            GroupLeg
                                { groupLeg = This iL
                                , groupTime = This tL
                                }
                    (Just (_, Nothing), Just (tL, Just iL)) ->
                        Just
                            GroupLeg
                                { groupLeg = This iL
                                , groupTime = This tL
                                }
                    (Just (tR, Just iR), Nothing) ->
                        Just
                            GroupLeg
                                { groupLeg = That iR
                                , groupTime = That tR
                                }
                    (Just (tR, Just iR), Just (_, Nothing)) ->
                        Just
                            GroupLeg
                                { groupLeg = That iR
                                , groupTime = That tR
                                }

            in (g, mf{fixes = ysL})

        | ysL <- yssL
        , let zerothL =
                case ysL of
                    [] ->
                        Nothing
                    (zerothFixL : _) ->
                        let t = fixToUtc mark0 zerothFixL in
                        Just (t, Map.lookup (Just t) timeToLeg)

        | ysR <- yssR
        , let nthR =
                case reverse ysR of
                    [] ->
                        Nothing
                    (nthFixR : _) ->
                        let t = fixToUtc mark0 nthFixR in
                        Just (t, Map.lookup (Just t) timeToLeg)
        ]
        where
            tagZs = tagZones @Double @Double e sp
            madeZs = madeZones @Double @Double e give
            fromZs = fromZones @Double @Double e give

            FlyCut{uncut = mf@MarkedFixes{mark0, fixes}} = clipToCut flyCut

            xs :: [Maybe ZoneTag]
            xs =
                tagZs (fromZs zones)
                . unSelectedCrossings
                . selectedCrossings
                $ madeZs timechecks task mf

            ts :: [Maybe UTCTime]
            ts =
                [
                    join $ do
                        ZoneTag{cross = ZoneCross{crossingPair = xy}} <- x
                        case xy of
                          [_, Fix{time}] -> return $ Just time
                          _ -> return Nothing

                | x <- xs
              ]

            timeToLeg :: Map.Map (Maybe UTCTime) LegIdx
            timeToLeg = Map.fromList $ zip ts (LegIdx <$> [1..])

            -- WARNING: Pilots can end up with duplicate timestamps when they are
            -- logging at a sub-second rate. For IGC files the HMS and ss fields are
            -- in the same B record but in different locations. If the parser does
            -- not know about the sub-second field then it will parse multiple fixes
            -- with the same HMS time.
            uniqueFixes = nub fixes

            yssL :: [[Kml.Fix]]
            yssL =
                split
                    ( keepDelimsL
                    $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                    uniqueFixes

            yssR :: [[Kml.Fix]]
            yssR =
                split
                    ( keepDelimsR
                    $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                    uniqueFixes

module Flight.Fsdb.TaskArrival (parseAltArrivals) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes)
import Data.List (sortOn)

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc
    , xpWrap, xpElem, xpAttr
    , xpFilterAttr
    , xpPrim
    , xpPair
    , xpTextAttr, xpOption
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (&&&)
    , (>>>)
    , runX
    , no, yes
    , withValidate, withWarnings, withRemoveWS
    , readString
    , hasName
    , getChildren
    , getAttrValue
    , constA
    , listA
    , arr
    , deep
    , containing
    , orElse
    , hasAttr
    )

import Flight.Track.Arrival (TrackArrival(..), arrivalsByTime, arrivalsByRank)
import Flight.Comp (PilotId(..), Pilot(..), Tweak(..), Task(..))
import "flight-gap-allot" Flight.Score (ArrivalFraction(..))
import "flight-gap-math" Flight.Score (ArrivalPoints(..))
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Internal.Parse (parseUtcTime)

dToR :: Double -> Rational
dToR = toRational

xpRankScore :: PU (Maybe (ArrivalPoints, UTCTime))
xpRankScore =
    xpElem "FsResult"
    $ xpFilterAttr
        ( hasName "arrival_points"
        <+> hasName "finished_ss"
        )
    $ xpWrap
        -- WARNING: FsResult@finished_ss is only written by FS when arrival
        -- points are positive but airscore writes it empty, not parsable as a
        -- UTC time. Guard against that parse with the check on @arrival_points.
        ( \(a, es) -> (ArrivalPoints $ dToR a,) <$> (parseUtcTime <$> if a > 0 then es else Nothing)
        , \case
            Nothing -> (0, Nothing)
            Just (ArrivalPoints a, es) -> (fromRational a, Just $ show es)
        )
    $ xpPair
        (xpAttr "arrival_points" xpPrim)
        (xpOption $ xpTextAttr "finished_ss")

getScore
    :: ArrowXml a
    => [Pilot]
    -> a XmlTree [(Pilot, (ArrivalPoints, UTCTime))]
getScore pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getArrivalPoint
    >>> arr catMaybes
    where
        kps = keyPilots pilots

        -- <FsParticipant id="50">
        --    <FsResult arrival_points="99.9" finished_ss="2013-12-30T17:32:02+11:00" />
        -- </FsParticipant>
        -- <FsParticipant id="22">
        --    <FsResult arrival_points="0" />
        -- </FsParticipant>
        getArrivalPoint =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getDidScore
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getDidScore =
                    getChildren
                    >>> hasName "FsParticipant"
                        `containing`
                        ( getChildren
                        >>> hasName "FsResult"
                        >>> hasAttr "arrival_points"
                        )
                    >>> getAttrValue "id"
                    &&& getResultScore
                    >>> arr (\(pid, x) -> do
                            y :: (Maybe (ArrivalPoints, UTCTime)) <- x
                            z :: (ArrivalPoints, UTCTime) <- y
                            return
                                (unKeyPilot (keyMap kps) . PilotId $ pid
                                , z
                                ))

                getResultScore =
                    getChildren
                    >>> hasName "FsResult"
                    >>> arr (unpickleDoc xpRankScore)

parseAltArrivals :: [Task k] -> String -> IO (Either String [[(Pilot, TrackArrival)]])
parseAltArrivals tasks contents = do
    let doc =
            readString
                [ withValidate no
                , withWarnings no
                , withRemoveWS yes
                ]
                contents

    ps <- runX $ doc >>> getCompPilot
    xss <- runX $ doc >>> getScore ps

    let yss :: [[(Pilot, TrackArrival)]] =
            [
                let aRank = maybe True arrivalRank tweak
                    aTime = maybe False arrivalTime tweak
                    -- TODO: Use points to work out fraction as a check on FS.
                    ys = [(p, t) | (p, (_, t)) <- xs]
                in
                    case (aRank, aTime) of
                        (True, _) -> arrivalsByRank ys
                        (False, True) -> arrivalsByTime ys
                        -- NOTE: We're not using either kind of arrival
                        -- for points so zero the fraction.
                        (False, False) ->
                            [ (p, ta{frac = ArrivalFraction 0})
                            | (p, ta) <- arrivalsByRank ys
                            ]

            | Task{taskTweak = tweak} <- tasks
            | xs <- xss
            ]

    return . Right $ sortOn (rank . snd) <$> yss

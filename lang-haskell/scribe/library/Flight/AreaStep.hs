module Flight.AreaStep
    ( readCompBestDistances
    , readCompLeading
    , writeCompAreaStep
    ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Csv (EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (empty, fromList, toList)

import Flight.Track.Time
    ( AreaRow(..), TickRow(..)
    , LeadClose(..), LeadAllDown(..), LeadArrival(..), LeadingAreas(..)
    , AreaSteps
    , area2, allHeaders
    )
import Flight.Track.Mask (RaceTime(..))
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , CompInputFile(..)
    , DiscardFurtherFile(..)
    , DiscardFurtherDir(..)
    , AreaStepFile(..)
    , AreaStepDir(..)
    , RoutesLookupTaskDistance(..)
    , discardFurtherDir
    , discardFurtherPath
    , areaStepPath
    , compFileToCompDir
    , routeLengthOfSs
    )
import Flight.DiscardFurther (readDiscardFurther, readCompBestDistances)
import Flight.Score (LcPoint, Leg, LeadingArea2Units)

writeAreaStep :: AreaStepFile -> [String] -> Vector AreaRow -> IO ()
writeAreaStep (AreaStepFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs $ V.toList xs

writeCompAreaStep
    :: CompInputFile
    -> [IxTask]
    -> [[(Pilot, LeadingAreas (Vector AreaRow) (Maybe LcPoint))]]
    -> IO [[Maybe (Vector AreaRow)]]
writeCompAreaStep compFile tasks ass =
        sequence
        [ sequence $ (writePilotAreaStep compFile (const True) task) <$> as
        | task <- tasks
        | as <- ass
        ]

writePilotAreaStep
    :: CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Pilot, LeadingAreas (Vector AreaRow) (Maybe LcPoint))
    -> IO (Maybe (Vector AreaRow))
writePilotAreaStep compFile selectTask iTask@(IxTask i) (pilot, LeadingAreas{areaFlown = areaRows}) =
    if not (selectTask iTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    _ <- f areaRows
    return $ Just areaRows
    where
        f = writeAreaStep (AreaStepFile $ dOut </> file) allHeaders
        dir = compFileToCompDir compFile
        (AreaStepDir dOut, AreaStepFile file) = areaStepPath dir i pilot

readCompLeading
    :: AreaSteps LeadingArea2Units
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> [IxTask]
    -> [Int -> Leg]
    -> [Maybe RaceTime]
    -> [[Pilot]]
    -> IO [[(Pilot, LeadingAreas (Vector AreaRow) (Maybe LcPoint))]]
readCompLeading areaSteps lengths compFile select tasks toLegs raceTimes pilots =
    sequence
        [
            (readTaskLeading areaSteps lengths compFile select)
                task
                toLeg
                rt
                ps
        | task <- tasks
        | toLeg <- toLegs
        | rt <- raceTimes
        | ps <- pilots
        ]

readTaskLeading
    :: AreaSteps LeadingArea2Units
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> [Pilot]
    -> IO [(Pilot, LeadingAreas (Vector AreaRow) (Maybe LcPoint))]
readTaskLeading areaSteps lengths compFile select iTask@(IxTask i) toLeg raceTime ps =
    if not (select iTask) then return [] else do
    _ <- createDirectoryIfMissing True dOut
    xs <- mapM (readPilotLeading areaSteps lengths compFile iTask toLeg raceTime) ps
    return $ zip ps xs
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir dOut) = discardFurtherDir dir i

readPilotLeading
    :: AreaSteps LeadingArea2Units
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO (LeadingAreas (Vector AreaRow) (Maybe LcPoint))
readPilotLeading _ _ _ _ _ Nothing _ = return $ LeadingAreas V.empty Nothing Nothing
readPilotLeading
    areaSteps
    (RoutesLookupTaskDistance lookupTaskLength)
    compFile iTask@(IxTask i) toLeg
    (Just raceTime)
    pilot = do
    (_, rows :: Vector TickRow) <- readDiscardFurther (DiscardFurtherFile (dIn </> file))
    return $ f rows
    where
        f =
            (\LeadingAreas{areaFlown = af, areaBeforeStart = bs, areaAfterLanding = al} ->
                LeadingAreas
                    { areaFlown = af
                    , areaBeforeStart = bs
                    , areaAfterLanding = al
                    })
            . area2
                areaSteps
                toLeg
                lengthOfSs
                close
                down
                arrival

        dir = compFileToCompDir compFile
        (DiscardFurtherDir dIn, DiscardFurtherFile file) = discardFurtherPath dir i pilot
        lengthOfSs = (fmap routeLengthOfSs . ($ iTask)) =<< lookupTaskLength
        close = LeadClose <$> leadClose raceTime
        down = LeadAllDown <$> leadAllDown raceTime
        arrival = LeadArrival <$> leadArrival raceTime

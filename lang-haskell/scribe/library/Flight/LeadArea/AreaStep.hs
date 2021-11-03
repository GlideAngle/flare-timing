module Flight.LeadArea.AreaStep
    ( readCompBestDistances
    , readCompLeadingAreas
    , writeCompLeadingAreas
    ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Csv (EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (empty, toList)

import Flight.Track.Time
    ( AreaRow(..), TickRow(..)
    , LeadClose(..), LeadAllDown(..), LeadArrival(..), LeadingAreas(..)
    , AreaSteps, AreaHeader(..)
    , ticksToAreas, areaHeader
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
import "flight-gap-lead" Flight.Score (LcPoint, Leg, LeadingAreaUnits)

writeAreaStep :: AreaStepFile -> Vector (AreaRow u) -> IO ()
writeAreaStep (AreaStepFile path) xs =
    L.writeFile path rows
    where
        (AreaHeader hs) = areaHeader
        opts = defaultEncodeOptions {encUseCrLf = False}
        rows = encodeByNameWith opts hs $ V.toList xs

writeCompLeadingAreas
    :: CompInputFile
    -> [IxTask]
    -> [[(Pilot, LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint))]]
    -> IO [[Maybe (Vector (AreaRow u))]]
writeCompLeadingAreas compFile tasks ass =
        sequence
        [ sequence $ writePilotAreaStep compFile (const True) task <$> as
        | task <- tasks
        | as <- ass
        ]

writePilotAreaStep
    :: CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Pilot, LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint))
    -> IO (Maybe (Vector (AreaRow u)))
writePilotAreaStep compFile selectTask ixTask (pilot, LeadingAreas{areaFlown = areaRows}) =
    if not (selectTask ixTask) then return Nothing else do
    _ <- createDirectoryIfMissing True dOut
    _ <- f areaRows
    return $ Just areaRows
    where
        f = writeAreaStep (AreaStepFile $ dOut </> file)
        dir = compFileToCompDir compFile
        (AreaStepDir dOut, AreaStepFile file) = areaStepPath dir ixTask pilot

readCompLeadingAreas
    :: AreaSteps (LeadingAreaUnits u)
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> [IxTask]
    -> [Int -> Leg]
    -> [Maybe RaceTime]
    -> [[Pilot]]
    -> IO [[(Pilot, LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint))]]
readCompLeadingAreas areaSteps lengths compFile select tasks toLegs raceTimes pilots =
    sequence
        [
            readTaskLeadingAreas areaSteps lengths compFile select task toLeg rt ps
        | task <- tasks
        | toLeg <- toLegs
        | rt <- raceTimes
        | ps <- pilots
        ]

readTaskLeadingAreas
    :: AreaSteps (LeadingAreaUnits u)
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> [Pilot]
    -> IO [(Pilot, LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint))]
readTaskLeadingAreas areaSteps lengths compFile select ixTask toLeg raceTime ps =
    if not (select ixTask) then return [] else do
    _ <- createDirectoryIfMissing True dOut
    xs <- mapM (readPilotLeadingAreas areaSteps lengths compFile ixTask toLeg raceTime) ps
    return $ zip ps xs
    where
        dir = compFileToCompDir compFile
        (DiscardFurtherDir dOut) = discardFurtherDir dir ixTask

readPilotLeadingAreas
    :: AreaSteps (LeadingAreaUnits u)
    -> RoutesLookupTaskDistance
    -> CompInputFile
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO (LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint))
readPilotLeadingAreas _ _ _ _ _ Nothing _ = return $ LeadingAreas V.empty Nothing Nothing
readPilotLeadingAreas
    areaSteps
    (RoutesLookupTaskDistance lookupTaskLength)
    compFile ixTask toLeg
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
            . ticksToAreas
                areaSteps
                toLeg
                lengthOfSs
                close
                down
                arrival

        dir = compFileToCompDir compFile
        (DiscardFurtherDir dIn, DiscardFurtherFile file) = discardFurtherPath dir ixTask pilot
        lengthOfSs = (fmap routeLengthOfSs . ($ ixTask)) =<< lookupTaskLength
        close = LeadClose <$> leadClose raceTime
        down = LeadAllDown <$> leadAllDown raceTime
        arrival = LeadArrival <$> leadArrival raceTime

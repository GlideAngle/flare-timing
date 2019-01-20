{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Lens ((^?), element)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Control.Monad (mapM_, when, zipWithM_)
import Control.Exception.Safe (catchIO)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Track.Time (LeadTick(..), RaceTick(..), TimeRow(..))
import Flight.Comp
    ( FileType(CompInput)
    , AlignDir(..)
    , CompInputFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , AlignTimeFile(..)
    , CompSettings(..)
    , PilotName(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail
    , FirstLead(..)
    , FirstStart(..)
    , OpenClose(..)
    , compToCross
    , crossToTag
    , compFileToCompDir
    , alignPath
    , findCompInput
    , openClose
    , ensureExt
    , pilotNamed
    )
import qualified Flight.Mask as Mask (Sliver(..))
import Flight.Mask
    ( FnIxTask, RaceSections(..), Ticked
    , checkTracks, groupByLeg, dashDistancesToGoal
    )
import Flight.Track.Cross (FlyingSection, Fix(..), TrackFlyingSection(..))
import Flight.Track.Mask (FlyCut(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), firstLead, firstStart)
import Flight.Kml (MarkedFixes(..))
import Data.Ratio.Rounding (dpRound)
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Scribe (readComp, readCrossing, readTagging, writeAlignTime)
import Flight.Lookup.Cross
    (FlyingLookup(..), crossFlying)
import Flight.Lookup.Tag
    (TickLookup(..), TagLookup(..), tagTicked, tagPilotTag)
import AlignTimeOptions (description)
import Flight.Span.Double (zoneToCylF, spanF, csF, cutF, dppF, csegF)

type Leg = Int

unTaskDistance :: QTaskDistance Double [u| m |] -> Double
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

headers :: [String]
headers = ["leg", "time", "lat", "lng", "tickLead", "tickRace", "distance"]

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Aligning times completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    crossing <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    let flyingLookup = crossFlying crossing

    case (compSettings, crossing, tagging) of
        (Nothing, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the crossings."
        (_, _, Nothing) -> putStrLn "Couldn't read the taggings."
        (Just cs, Just _, Just t) ->
            let f =
                    writeTime
                        (IxTask <$> task)
                        (pilotNamed cs $ PilotName <$> pilot)
                        (CompInputFile compPath)
            in (f . checkAll speedSectionOnly flyingLookup) t

writeTime
    :: [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> (CompInputFile
      -> [IxTask]
      -> [Pilot]
      -> IO [[Either (Pilot, t) (Pilot, Pilot -> [TimeRow])]])
    -> IO ()
writeTime selectTasks selectPilots compFile f = do
    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just xs -> do
            let ys :: [[(Pilot, [TimeRow])]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, [])
                            Right (p, g) -> (p, g p))
                        xs

            zipWithM_
                (\ n zs ->
                    when (includeTask selectTasks $ IxTask n) $
                        mapM_ (writePilotTimes compFile n) zs)
                [1 .. ]
                ys

checkAll
    :: Bool -- ^ Exclude zones outside speed section
    -> FlyingLookup
    -> Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO
         [
             [Either
                 (Pilot, TrackFileFail)
                 (Pilot, Pilot -> [TimeRow])
             ]
         ]
checkAll ssOnly flyingLookup tagging =
    checkTracks (\CompSettings{tasks} -> group ssOnly flyingLookup tagging tasks)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

writePilotTimes :: CompInputFile -> Int -> (Pilot, [TimeRow]) -> IO ()
writePilotTimes compFile iTask (pilot, rows) = do
    _ <- createDirectoryIfMissing True dOut
    _ <- writeAlignTime (AlignTimeFile $ dOut </> f) headers rows
    return ()
    where
        dir = compFileToCompDir compFile
        (AlignDir dOut, AlignTimeFile f) = alignPath dir iTask pilot

mkTimeRows
    :: Maybe FirstLead
    -> Maybe FirstStart
    -> Leg
    -> Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
    -> [TimeRow]
mkTimeRows _ _ _ Nothing = []
mkTimeRows lead start leg (Just xs) =
    catMaybes $ mkTimeRow lead start leg <$> xs

mkTimeRow
    :: Maybe FirstLead
    -> Maybe FirstStart
    -> Int
    -> (Maybe Fix, Maybe (QTaskDistance Double [u| m |]))
    -> Maybe TimeRow
mkTimeRow Nothing _ _ _ = Nothing
mkTimeRow _ _ _ (Nothing, _) = Nothing
mkTimeRow _ _ _ (_, Nothing) = Nothing
mkTimeRow lead start leg (Just Fix{time, lat, lng}, Just d) =
    Just
        TimeRow
            { leg = leg

            , tickLead =
                LeadTick
                . realToFrac
                . (\(FirstLead l) -> time `diffUTCTime` l)
                <$> lead

            , tickRace =
                RaceTick
                . realToFrac
                . (\(FirstStart s) -> time `diffUTCTime` s)
                <$> start

            , time = time
            , lat = lat
            , lng = lng
            , distance = unTaskDistance d
            }

group
    :: Bool -- ^ Exclude zones outside speed section
    -> FlyingLookup
    -> Tagging
    -> FnIxTask k (Pilot -> [TimeRow])
group
    ssOnly
    (FlyingLookup lookupFlying)
    tags@Tagging{timing}
    tasks iTask@(IxTask i)
    mf@MarkedFixes{mark0} p =
    case (tasks ^? element (i - 1), timing ^? element (i - 1)) of
        (_, Nothing) -> []
        (Nothing, _) -> []
        (Just Task{speedSection = Nothing}, _) -> []
        (Just task@Task{speedSection = ss@(Just (start, end))}, Just times) ->
            maybe
                zs
                ( maybe zs (\z -> zs ++ [z])
                . (\f ->
                    mkTimeRow
                        firstLead'
                        firstStart'
                        end
                        (Just f, Just $ TaskDistance [u| 0m |]))
                )
                endZoneTag
            where
                scoredRange :: FlyingSection UTCTime =
                    fromMaybe
                        (Just (mark0, mark0))
                        (fmap scoredTimes . (\f -> f iTask p) =<< lookupFlying)

                -- NOTE: Ensure we're only considering scored subset of flying time.
                scoredFixes =
                    FlyCut
                        { cut = scoredRange
                        , uncut = mf
                        }

                firstTimes = zonesFirst times

                firstLead' = firstLead ss firstTimes

                firstStart' =
                    (\OpenClose{open} -> firstStart ss open firstTimes)
                    =<< openClose ss (zoneTimes task)

                xs :: [MarkedFixes]
                xs = groupByLeg spanF zoneToCylF task scoredFixes

                ys = FlyCut scoredRange <$> xs

                ticked =
                    fromMaybe (RaceSections [] [] [])
                    $ (\f -> f iTask ss p mf) =<< lookupTicked

                endZoneTag :: Maybe Fix
                endZoneTag = do
                    ts :: [Maybe Fix]
                        <- (\f -> f iTask ss p mf) =<< lookupZoneTags

                    us :: [Fix]
                        <- sequence ts

                    listToMaybe . take 1 . drop (end - start) $ us

                zs :: [TimeRow]
                zs =
                    concat $ zipWith
                        (\j x ->
                            let (leg, reticked) = retick ticked start j
                            in legDistances ssOnly reticked times task leg x)
                        [0 .. ]
                        ys
    where
        (TickLookup lookupTicked) = tagTicked (Just tags)
        (TagLookup lookupZoneTags) = tagPilotTag (Just tags)

-- | For a given leg, only so many race zones can be ticked.
retick :: Ticked -> Int -> Int -> (Int, Ticked)
retick rs@RaceSections{prolog, race} start leg =
    (leg + delta, rs')
    where
        -- NOTE: Some pilots get towed up outside the start circle. Make an
        -- adjustment between the start and the zones in the prolog ticked.
        delta = (start - 1) - length prolog
        rs' = rs { race = take (leg - start + 1) race }

allLegDistances
    :: Ticked
    -> TrackTime
    -> Task k
    -> Leg
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
allLegDistances ticked times task@Task{speedSection, zoneTimes} leg xs =
    mkTimeRows lead start leg xs'
    where
        xs' :: Maybe [(Maybe Fix, Maybe (QTaskDistance Double [u| m |]))]
        xs' = dashDistancesToGoal ticked sliver zoneToCylF task xs

        sliver = Mask.Sliver spanF dppF csegF csF cutF
        ts = zonesFirst times

        lead = firstLead speedSection ts

        start =
            (\OpenClose{open} -> firstStart speedSection open ts)
            =<< openClose speedSection zoneTimes

legDistances
    :: Bool -- ^ Exclude zones outside speed section
    -> Ticked
    -> TrackTime
    -> Task k
    -> Leg
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
legDistances False ticked times task leg xs=
    allLegDistances ticked times task leg xs

legDistances True ticked times task@Task{speedSection} leg xs =
    if excludeLeg then [] else allLegDistances ticked times task leg xs
    where
        leg' = fromIntegral leg

        excludeLeg =
            maybe
                False
                (\(start, end) -> leg' < start || leg' > end)
                speedSection

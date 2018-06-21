{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Prelude hiding (span)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Lens ((^?), element)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Control.Monad (join, mapM_, when, zipWithM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Flight.Track.Time (LeadTick(..), RaceTick(..), TimeRow(..))
import Flight.Comp
    ( FileType(CompInput)
    , AlignDir(..)
    , CompInputFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , AlignTimeFile(..)
    , CompSettings(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail
    , FlyingSection
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
    )
import Flight.Units ()
import qualified Flight.Mask as Mask (Sliver(..))
import Flight.Mask
    ( FnIxTask, RaceSections(..), FlyCut(..), Ticked
    , checkTracks, groupByLeg, dashDistancesToGoal
    )
import Flight.Track.Cross (Fix(..), TrackFlyingSection(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), firstLead, firstStart)
import Flight.Kml (MarkedFixes(..))
import Data.Number.Rounding (dpRound)
import Flight.Distance (TaskDistance(..))
import Flight.Scribe (readCrossing, readTagging, writeAlignTime)
import Flight.Lookup.Cross
    (FlyingLookup(..), crossFlying)
import Flight.Lookup.Tag
    (TickLookup(..), TagLookup(..), tagTicked, tagPilotTag)
import AlignTimeOptions (description)
import Flight.Span.Double (zoneToCylF, spanF, csF, cutF, dppF, csegF)

type Leg = Int

unTaskDistance :: (Real a, Fractional a) => TaskDistance a -> a
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

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Aligning times completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    crossing <- runExceptT $ readCrossing crossFile
    tagging <- runExceptT $ readTagging tagFile

    let flyingLookup = crossFlying crossing

    case (crossing, tagging) of
        (Left msg, _) -> putStrLn msg
        (_, Left msg) -> putStrLn msg
        (Right _, Right t) -> (f . checkAll speedSectionOnly flyingLookup) t

    where
        f = writeTime (IxTask <$> task) (Pilot <$> pilot) (CompInputFile compPath)

writeTime :: [IxTask]
          -> [Pilot]
          -> CompInputFile
          -> (CompInputFile
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                  String IO [[Either (Pilot, t) (Pilot, Pilot -> [TimeRow])]])
          -> IO ()
writeTime selectTasks selectPilots compFile f = do
    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right xs -> do
            let ys :: [[(Pilot, [TimeRow])]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, [])
                            Right (p, g) -> (p, g p))
                        xs

            _ <- zipWithM_
                (\ n zs ->
                    when (includeTask selectTasks $ IxTask n) $
                        mapM_ (writePilotTimes compFile n) zs)
                [1 .. ]
                ys

            return ()

checkAll
    :: Bool -- ^ Exclude zones outside speed section
    -> FlyingLookup
    -> Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> ExceptT
         String
         IO
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

mkTimeRows :: Maybe FirstLead
           -> Maybe FirstStart
           -> Leg
           -> Maybe [(Maybe Fix, Maybe (TaskDistance Double))]
           -> [TimeRow]
mkTimeRows _ _ _ Nothing = []
mkTimeRows lead start leg (Just xs) =
    catMaybes $ mkTimeRow lead start leg <$> xs

mkTimeRow :: Maybe FirstLead
          -> Maybe FirstStart
          -> Int
          -> (Maybe Fix, Maybe (TaskDistance Double))
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
    -> FnIxTask (Pilot -> [TimeRow])
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
                flyingRange :: FlyingSection UTCTime =
                    fromMaybe (Just (mark0, mark0))
                    $ join (fmap flyingTimes . (\f -> f iTask p) <$> lookupFlying)

                -- NOTE: Ensure we're only considering flying time.
                flyFixes =
                    FlyCut
                        { cut = flyingRange
                        , uncut = mf
                        }

                firstTimes = zonesFirst times

                firstLead' = firstLead ss firstTimes

                firstStart' =
                    join
                    $ (\OpenClose{open} -> firstStart ss open firstTimes)
                    <$> openClose ss (zoneTimes task)

                xs :: [MarkedFixes]
                xs = groupByLeg spanF zoneToCylF task flyFixes

                ys = FlyCut flyingRange <$> xs

                ticked =
                    fromMaybe (RaceSections [] [] [])
                    $ join ((\f -> f iTask ss p mf) <$> lookupTicked)

                endZoneTag :: Maybe Fix
                endZoneTag = do
                    ts :: [Maybe Fix]
                        <- join ((\f -> f iTask ss p mf) <$> lookupZoneTags)

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
        (TickLookup lookupTicked) = tagTicked (Right tags)
        (TagLookup lookupZoneTags) = tagPilotTag (Right tags)

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
    -> Task
    -> Leg
    -> FlyCut UTCTime MarkedFixes
    -> [TimeRow]
allLegDistances ticked times task@Task{speedSection, zoneTimes} leg xs =
    mkTimeRows lead start leg xs'
    where
        sliver = Mask.Sliver spanF dppF csegF csF cutF
        xs' = dashDistancesToGoal ticked sliver zoneToCylF task xs
        ts = zonesFirst times

        lead = firstLead speedSection ts

        start =
            join
            $ (\OpenClose{open} -> firstStart speedSection open ts)
            <$> openClose speedSection zoneTimes

legDistances
    :: Bool -- ^ Exclude zones outside speed section
    -> Ticked
    -> TrackTime
    -> Task
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

{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.Vector (Vector)
import System.FilePath ((</>), (<.>) , takeFileName, takeBaseName, takeDirectory)
import Control.Exception.Safe (catchIO)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden (goldenVsFile)

import Flight.Kml (MarkedFixes(..))
import Flight.Track.Tag (Tagging(..))
import Flight.Track.Time (TrackRow(..), allHeaders)
import Flight.Comp
    ( CompInputFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , UnpackTrackFile(..)
    , AlignTimeFile(..)
    , CompSettings(..)
    , PilotName(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , compToCross
    , crossToTag
    , pilotNamed
    )
import qualified Flight.Scribe as Scribe
import Flight.Lookup.Cross (FlyingLookup(..), crossFlying)
import Flight.Time.Align (group)

main :: IO ()
main =
    defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
    ci : _ <- findByExtension [".comp-input.yaml"] "test/golden/test-files/real-world/Forbes/2012"
    rs <- findByExtension [".csv"] "test/golden/test-files/real-world/Forbes/2012/.flare-timing/unpack-track"
    rg <- goldenTestSet "real-world examples" (CompInputFile ci) rs
    return $ testGroup "golden tests" [rg]

goldenTestSet :: String -> CompInputFile -> [FilePath] -> IO TestTree
goldenTestSet title compFile@(CompInputFile compPath) unpackTrackFiles = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> Scribe.readComp compFile)
            (const $ return Nothing)

    crossing <-
        catchIO
            (Just <$> Scribe.readCrossing crossFile)
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> Scribe.readTagging tagFile)
            (const $ return Nothing)

    let flyingLookup = crossFlying crossing

    case (compSettings, crossing, tagging) of
        (Nothing, _, _) -> do
            putStrLn "Couldn't read the comp settings."
            return $ testGroup "" []
        (_, Nothing, _) -> do
            putStrLn "Couldn't read the crossings."
            return $ testGroup "" []
        (_, _, Nothing) -> do
            putStrLn "Couldn't read the taggings."
            return $ testGroup "" []
        (Just cs, Just _, Just t) ->
            return $ testGroup title 
                [ testGroup "unpack-track to align-time"
                    [ goldenVsFile
                        (testName unpackTrackFile)
                        (alignTimeFile <.> ".golden")
                        alignTimeFile
                        (writeAlignTime tsks ix p flyingLookup t unpackTrackFile)
                    | unpackTrackFile <- unpackTrackFiles
                    , let alignTimeFile = alignTimePath unpackTrackFile
                    , let (p : _) = pilotNamed cs [PilotName "Rohan Holtkamp"]
                    , let tsks = tasks cs
                    , let ix = IxTask 1
                    ]
                ]

testName :: FilePath -> FilePath
testName = takeBaseName

alignTimePath :: FilePath -> FilePath
alignTimePath unpackTrackFile =
    dir </> "align-time" </> "task-1" </> fn
    where
        fn = takeFileName unpackTrackFile 
        dir = takeDirectory . takeDirectory . takeDirectory $ unpackTrackFile 

writeAlignTime :: [Task k] -> IxTask -> Pilot -> FlyingLookup -> Tagging -> FilePath -> IO ()
writeAlignTime tasks ix p lookupFlying tags unpackTrackFile = do
    unpackTrack <-
        catchIO
            (Just <$> Scribe.readUnpackTrack inFile)
            (const $ return Nothing)

    case unpackTrack of
        Nothing -> putStrLn $ "Couldn't read the file " ++ unpackTrackFile
        Just (_, rows) -> do
            let mf = trackToMarkedFixes rows
            let xs = group False lookupFlying tags tasks ix mf p
            _ <- Scribe.writeAlignTime outFile allHeaders xs
            return ()
    where
        inFile = UnpackTrackFile unpackTrackFile
        outFile = AlignTimeFile $ alignTimePath unpackTrackFile

trackToMarkedFixes :: Vector TrackRow -> MarkedFixes
trackToMarkedFixes = undefined
import Text.Printf (printf)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseTweak
    , parseScoreBack
    , parseTasks
    , parseTaskPilotGroups
    , parseTaskFolders
    , parseTracks
    )
import Flight.Comp
    ( FileType(Fsdb)
    , FsdbFile(..)
    , FsdbXml(..)
    , CompSettings(..)
    , EarthMath(..)
    , Comp(..)
    , Nominal(..)
    , Tweak(..)
    , Task(..)
    , TaskFolder(..)
    , PilotGroup(..)
    , PilotTrackLogFile(..)
    , fsdbToComp
    , findFsdb
    , ensureExt
    )
import qualified Flight.Comp as C (Comp(earth, earthMath))
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..), center)
import qualified Flight.Zone.Raw as Raw (RawZone(..), Give(..), zoneGive)
import Flight.Zone.MkZones (Discipline(..), Zones(..))
import Flight.Score (ScoreBackTime(..))
import Flight.Scribe (writeComp)
import Flight.Mask (separatedRawZones)
import ExtractInputOptions (CmdOptions(..), mkOptions, mkEarthModel)

import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import qualified Flight.Earth.Sphere.Separated as S (separatedZones)
import Flight.Track.Lead (lwScalingDefault)

spanD :: SpanLatLng Double
spanD = Dbl.distanceHaversine

sepD :: [Zone Double] -> Bool
sepD = S.separatedZones spanD

separated :: Raw.RawZone -> Raw.RawZone -> Bool
separated =
    separatedRawZones f
    where
        f x y = sepD [x, y] && sepD [Point $ center x, y]

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name

    let lf = LenientFile {coerceFile = ensureExt Fsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive CmdOptions{giveFraction = Nothing} =
    fail
    $ "Please supply give for the tolerance around control zones."
    ++ " Flag --give-fraction is required and"
    ++ " flag --give-distance is optional."
drive
    o@CmdOptions
        { giveFraction = Just gf
        , giveDistance = gd
        , earthMath = dm
        }
    = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findFsdb o

    putStrLn $ "Using a give fraction of " ++ printf "%.5f" gf
    case gd of
        Nothing -> 
            putStrLn "The give distance was not supplied"
        Just gd' ->
            putStrLn $ "Using a give distance of " ++ printf "%.3f" gd' ++ " m"

    let give =
            Raw.Give
                { giveFraction = gf
                , giveDistance = Radius . MkQuantity <$> gd
                }

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go dm give) files
    end <- getTime Monotonic
    fprint ("Extracting tasks completed in " % timeSpecs % "\n") start end

go :: EarthMath -> Raw.Give -> FsdbFile -> IO ()
go dm zg fsdbFile@(FsdbFile fsdbPath) = do
    contents <- readFile fsdbPath
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ fsdbSettings dm zg (FsdbXml contents')
    either print (writeComp (fsdbToComp fsdbFile)) settings

fsdbComp :: FsdbXml -> ExceptT String IO Comp
fsdbComp (FsdbXml contents) = do
    cs <- lift $ parseComp contents
    case cs of
        Left msg -> ExceptT . return $ Left msg
        Right [c] -> ExceptT . return $ Right c
        Right _ -> do
            let msg = "Expected only one comp"
            lift $ print msg
            throwE msg

fsdbNominal :: FsdbXml -> ExceptT String IO Nominal
fsdbNominal (FsdbXml contents) = do
    ns <- lift $ parseNominal contents
    case ns of
        Left msg -> ExceptT . return $ Left msg
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbTweak :: Discipline -> FsdbXml -> ExceptT String IO Tweak
fsdbTweak discipline (FsdbXml contents) = do
    ns <- lift $ parseTweak discipline contents
    case ns of
        Left msg -> ExceptT . return $ Left msg
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of tweaks for the comp"
            lift $ print msg
            throwE msg

fsdbScoreBack
    :: FsdbXml
    -> ExceptT String IO (Maybe (ScoreBackTime (Quantity Double [u| s |])))
fsdbScoreBack (FsdbXml contents) = do
    xs <- lift $ parseScoreBack contents
    case xs of
        Left msg -> ExceptT . return $ Left msg
        Right [] -> ExceptT . return $ Right Nothing
        Right [x] -> ExceptT . return $ Right x
        _ -> do
            let msg = "Expected one or no score back time for the comp"
            lift $ print msg
            throwE msg

fsdbTasks
    :: Discipline
    -> Maybe (ScoreBackTime (Quantity Double [u| s |]))
    -> FsdbXml
    -> ExceptT String IO [Task k]
fsdbTasks discipline sb (FsdbXml contents) = do
    ts <- lift $ parseTasks discipline sb contents
    ExceptT $ return ts

fsdbTaskPilotGroups :: FsdbXml -> ExceptT String IO [PilotGroup]
fsdbTaskPilotGroups (FsdbXml contents) = do
    ts <- lift $ parseTaskPilotGroups contents
    ExceptT $ return ts

fsdbTaskFolders :: FsdbXml -> ExceptT String IO [TaskFolder]
fsdbTaskFolders (FsdbXml contents) = do
    fs <- lift $ parseTaskFolders contents
    ExceptT $ return fs

fsdbTracks :: FsdbXml -> ExceptT String IO [[PilotTrackLogFile]]
fsdbTracks (FsdbXml contents) = do
    fs <- lift $ parseTracks contents
    ExceptT $ return fs

fsdbSettings
    :: EarthMath
    -> Raw.Give
    -> FsdbXml
    -> ExceptT String IO (CompSettings k)
fsdbSettings dm zg fsdbXml = do
    c@Comp{discipline = hgOrPg} <- fsdbComp fsdbXml
    n <- fsdbNominal fsdbXml
    sb <- fsdbScoreBack fsdbXml
    ts <- fsdbTasks hgOrPg sb fsdbXml
    pgs <- fsdbTaskPilotGroups fsdbXml
    fs <- fsdbTaskFolders fsdbXml
    tps <- fsdbTracks fsdbXml

    let ts' =
            [ t{zones = z{raw = Raw.zoneGive separated zg rz}}
            | t@Task{zones = z@Zones{raw = rz}} <- ts
            ]

    tw@Tweak{leadingWeightScaling = lw} <- fsdbTweak hgOrPg fsdbXml
    let lw' = if Just (lwScalingDefault hgOrPg) == lw then Nothing else lw

    let msg =
            "Extracted "
            ++ show (length ts)
            ++ " tasks from \""
            ++ compName c
            ++ "\""

    lift . putStrLn $ msg
    return
        CompSettings
            { comp =
                c
                    { scoreBack = sb
                    , give = Just zg
                    , C.earth = mkEarthModel dm
                    , C.earthMath = dm

                    }
            , nominal = n
            , tweak = tw{ leadingWeightScaling = lw' }
            , tasks = ts'
            , taskFolders = fs
            , pilots = tps
            , pilotGroups = pgs
            }

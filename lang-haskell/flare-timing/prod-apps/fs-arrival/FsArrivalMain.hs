{-# LANGUAGE CPP #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
#if __GLASGOW_HASKELL__ <= 802
import Control.Monad (mapM_)
#endif
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb
    ( parseComp
    , parseTweak
    , parseScoreBack
    , parseTasks
    , parseAltArrivals
    )
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Mask (CompMaskingArrival(..))
import Flight.Comp
    ( AltDot(AltFs)
    , FindDirFile(..)
    , FileType(TrimFsdb)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , Pilot(..)
    , Comp(..)
    , Task(..)
    , Tweak(..)
    , trimFsdbToAltArrival
    , findTrimFsdb
    , reshape
    )
import Flight.Zone.MkZones (Discipline(..))
import "flight-gap-allot" Flight.Score (PilotsAtEss(..))
import "flight-gap-stop" Flight.Score (ScoreBackTime(..))
import Flight.Scribe (readTrimFsdb, writeAltArrival)
import FsArrivalOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape TrimFsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findTrimFsdb $ FindDirFile {dir = cwd, file = file}

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative arrivals completed in " % timeSpecs % "\n") start end

go :: TrimFsdbFile -> IO ()
go trimFsdbFile = do
    FsdbXml contents <- readTrimFsdb trimFsdbFile
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ normArrivals (FsdbXml contents')
    either print (writeAltArrival (trimFsdbToAltArrival AltFs trimFsdbFile)) settings

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
    -> Maybe Tweak
    -> Maybe (ScoreBackTime (Quantity Double [u| s |]))
    -> FsdbXml
    -> ExceptT String IO [Task k]
fsdbTasks discipline tw sb (FsdbXml contents) = do
    ts <- lift $ parseTasks discipline tw sb contents
    ExceptT $ return ts

fsdbArrivals :: [Task k] -> FsdbXml -> ExceptT String IO [[(Pilot, TrackArrival)]]
fsdbArrivals tasks (FsdbXml contents) = do
    fs <- lift $ parseAltArrivals tasks contents
    ExceptT $ return fs

normArrivals :: FsdbXml -> ExceptT String IO CompMaskingArrival
normArrivals fsdbXml = do
    Comp{discipline = hgOrPg} <- fsdbComp fsdbXml
    tw <- Just <$> fsdbTweak hgOrPg fsdbXml
    sb <- fsdbScoreBack fsdbXml
    ts <- fsdbTasks hgOrPg tw sb fsdbXml
    as <- fsdbArrivals ts fsdbXml
    return
        CompMaskingArrival
            { pilotsAtEss = PilotsAtEss . toInteger . length <$> as
            , arrivalRank = as
            }

module Flight.Mask.Bonus
    ( readTaskMaskBonus, writeTaskMaskBonus
    , readCompMaskBonus, writeCompMaskBonus
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Mask
    (TaskMaskingReach(..), CompMaskingReach(..), mkCompMaskReach, unMkCompMaskReach)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), MaskBonusFile(..), compFileToTaskFiles, taskToMaskBonus)

readTaskMaskBonus :: MonadIO m => MaskBonusFile -> m TaskMaskingReach
readTaskMaskBonus (MaskBonusFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskBonus :: MaskBonusFile -> TaskMaskingReach -> IO ()
writeTaskMaskBonus (MaskBonusFile path) bonusReach = do
    let cfg = Y.setConfCompare (fieldOrder bonusReach) Y.defConfig
    let yaml = Y.encodePretty cfg bonusReach
    BS.writeFile path yaml

readCompMaskBonus :: CompInputFile -> IO CompMaskingReach
readCompMaskBonus compFile = do
    putStrLn "Reading bonus reach (for stopped tasks) from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskReach
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskBonusFile
                readTaskMaskBonus maskBonusFile
            | maskBonusFile <- taskToMaskBonus <$> taskFiles
            ]

writeCompMaskBonus :: CompInputFile -> CompMaskingReach -> IO ()
writeCompMaskBonus compFile compMaskBonus = do
    putStrLn "Writing bonus reach (for stopped tasks) to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskBonusFile
            writeTaskMaskBonus maskBonusFile taskMaskBonus

        | taskMaskBonus <- unMkCompMaskReach compMaskBonus
        | maskBonusFile <- taskToMaskBonus <$> taskFiles
        ]

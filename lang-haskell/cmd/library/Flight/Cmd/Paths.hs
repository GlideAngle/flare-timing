module Flight.Cmd.Paths (LenientFile(..), checkPaths) where

import GHC.Records
import Control.Monad.Except (liftIO, throwError, when, unless)
import Control.Monad.Trans.Except (runExceptT)
import System.Directory (doesFileExist)

-- | Some function that modifies a file name so that would pass @doesFileExist@
-- if indeed the file exists. For example this might allow specifying the base
-- name of the file without the extension. Pass @id@ if you require the full
-- file name.
newtype LenientFile =
    LenientFile { coerceFile :: FilePath -> FilePath }

-- SEE: http://stackoverflow.com/questions/2138819/in-haskell-is-there-a-way-to-do-io-in-a-function-guard
checkPaths
    :: HasField "file" o String
    => LenientFile
    -> o
    -> IO (Maybe String)
checkPaths LenientFile{coerceFile} o = do
    x <- runExceptT $ do
        when (file == "") (throwError "No --file argument")

        dfe <- liftIO $ doesFileExist file'
        unless
            dfe
            (throwError . mconcat $
                [ "The --file argument is not a file."
                , if file /= file'
                     then " The file checked for was '" ++ file' ++ "'."
                     else ""
                ])

    return $ either Just (const Nothing) x
    where
        file = getField @"file" o
        file' = coerceFile file

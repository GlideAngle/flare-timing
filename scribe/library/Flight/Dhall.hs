module Flight.Dhall
    ( renderDhall
    ) where

import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Lens.Micro ((^.), set)
import System.FilePath (takeDirectory)
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bifunctor (first)
import Data.Aeson (ToJSON, Value)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.IO as T (readFile)
import Dhall
    ( InputSettings, Text
    , rootDirectory, sourceName, defaultInputSettings
    )
import Dhall.Core (Expr)
import Dhall.Parser (Src, exprFromText)
import Dhall.Import (loadWith, emptyStatus)
import Dhall.TypeCheck (X, typeOf)
import Dhall.JSON (dhallToJSON)
import Dhall.Pretty (prettyExpr, layoutOpts)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

-- SEE: https://github.com/mstksg/hakyll-dhall
renderDhall :: (PP.Pretty a, Eq a) => Expr Src a -> T.Text
renderDhall =
    PP.renderStrict
    . PP.layoutSmart layoutOpts
    . PP.unAnnotate
    . prettyExpr

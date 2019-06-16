module FlareTiming.Task.Validity.Widget
    ( ElementId
    , spacer
    , elV
    , elN
    , elD
    ) where

import qualified Data.Text as T (Text)
import Reflex.Dom

type ElementId = T.Text

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "spacer" $ return ()

elV :: DomBuilder t m => T.Text -> m ()
elV = elClass "td" "validity" . text

elN :: DomBuilder t m => T.Text -> m ()
elN = elClass "td" "td-norm" . text

elD :: DomBuilder t m => T.Text -> m ()
elD = elClass "td" "td-norm td-diff" . text

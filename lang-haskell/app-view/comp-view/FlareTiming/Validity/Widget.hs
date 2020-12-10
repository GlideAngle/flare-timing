module FlareTiming.Validity.Widget
    ( ElementId
    , spacer
    , elV
    , elN
    , elD
    , elVSelect
    , elNSelect
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

elVSelect :: DomBuilder t m => T.Text -> m ()
elVSelect = elClass "th" "validity" . text

elNSelect :: DomBuilder t m => T.Text -> m ()
elNSelect = elClass "th" "td-norm" . text

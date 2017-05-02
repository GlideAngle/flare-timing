module EmbedString (embedStr) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

-- SEE: https://www.haskell.org/pipermail/haskell-cafe/2008-September/047384.html
embedStr :: IO String -> ExpQ
embedStr readStr = lift =<< runIO readStr

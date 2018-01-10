{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Distance.Relative (RelativeDifficulty(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

-- | The relative difficulty of a chunk.
newtype RelativeDifficulty = RelativeDifficulty Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces RelativeDifficulty where
    defdp _ = DecimalPlaces 8

instance Newtype RelativeDifficulty Rational where
    pack = RelativeDifficulty
    unpack (RelativeDifficulty a) = a

instance ToJSON RelativeDifficulty where
    toJSON x = toJSON $ ViaSci x

instance FromJSON RelativeDifficulty where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x

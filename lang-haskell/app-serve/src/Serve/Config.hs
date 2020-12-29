module Serve.Config (Config(..), AppT(..), nullConfig) where

import Data.UnitsOfMeasure (u)
import Servant (ServantErr)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Except (ExceptT(..), MonadError)

import Flight.Units ()
import qualified Flight.Track.Cross as Cg (Flying(..), Crossing(..))
import qualified Flight.Track.Tag as Tg (Tagging(..))
import qualified Flight.Track.Stop as Sp (Framing(..))
import Flight.Track.Land (Landing(..))
import Flight.Track.Lead (DiscardingLead(..))
import qualified Flight.Track.Mask as Mask (MaskingArrival(..))
import Flight.Track.Mask
    ( MaskingEffort(..)
    , MaskingLead(..)
    , MaskingReach(..)
    , MaskingSpeed(..)
    )
import qualified Flight.Track.Point as Alt (AltPointing(..))
import Flight.Track.Point (Pointing(..))
import "flight-gap-lead" Flight.Score (LeadingArea2Units)
import Flight.Comp (CompSettings(..), CompInputFile(..))
import Flight.Route (TaskTrack(..), GeoLines(..))

data Config k
    = Config
        { compFile :: CompInputFile
        , compSettings :: CompSettings k
        , routing :: Maybe [Maybe TaskTrack]
        , flying :: Maybe Cg.Flying
        , crossing :: Maybe Cg.Crossing
        , tagging :: Maybe Tg.Tagging
        , framing :: Maybe Sp.Framing
        , maskingArrival :: Maybe Mask.MaskingArrival
        , maskingEffort :: Maybe MaskingEffort
        , discardingLead2 :: Maybe (DiscardingLead LeadingArea2Units)
        , maskingLead :: Maybe (MaskingLead [u| (km^2)*s |] [u| 1/(km^2)*s |])
        , maskingReach :: Maybe MaskingReach
        , maskingSpeed :: Maybe MaskingSpeed
        , bonusReach :: Maybe MaskingReach
        , landing :: Maybe Landing
        , pointing :: Maybe Pointing
        , altFsArrival :: Maybe Mask.MaskingArrival
        , altFsLandout :: Maybe Landing
        , altFsRoute :: Maybe [GeoLines]
        , altFsScore :: Maybe Alt.AltPointing
        , altAsScore :: Maybe Alt.AltPointing
        }

nullConfig :: CompInputFile -> CompSettings k -> Config k
nullConfig cf cs =
    Config
        { compFile = cf
        , compSettings = cs
        , routing = Nothing
        , flying = Nothing
        , crossing = Nothing
        , tagging = Nothing
        , framing = Nothing
        , maskingArrival = Nothing
        , maskingEffort = Nothing
        , discardingLead2 = Nothing
        , maskingLead = Nothing
        , maskingReach = Nothing
        , maskingSpeed = Nothing
        , bonusReach = Nothing
        , landing = Nothing
        , pointing = Nothing
        , altFsArrival = Nothing
        , altFsLandout = Nothing
        , altFsRoute = Nothing
        , altFsScore = Nothing
        , altAsScore = Nothing
        }

newtype AppT k m a =
    AppT
        { unApp :: ReaderT (Config k) (ExceptT ServantErr m) a
        }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Config k)
        , MonadError ServantErr
        , MonadIO
        , MonadThrow
        )

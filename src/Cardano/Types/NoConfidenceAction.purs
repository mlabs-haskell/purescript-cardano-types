module Cardano.Types.NoConfidenceAction
  ( NoConfidenceAction(NoConfidenceAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype NoConfidenceAction = NoConfidenceAction
  { actionId :: Maybe GovernanceActionId
  }

derive instance Generic NoConfidenceAction _
derive instance Newtype NoConfidenceAction _
derive instance Eq NoConfidenceAction
derive instance Ord NoConfidenceAction
derive newtype instance EncodeAeson NoConfidenceAction
derive newtype instance DecodeAeson NoConfidenceAction

instance Show NoConfidenceAction where
  show = genericShow

instance AsCbor NoConfidenceAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: NoConfidenceAction -> Cdl.NoConfidenceAction
toCdl (NoConfidenceAction { actionId }) =
  maybe Cdl.noConfidenceAction_new
    (Cdl.noConfidenceAction_newWithActionId <<< GovernanceActionId.toCdl)
    actionId

fromCdl :: Cdl.NoConfidenceAction -> NoConfidenceAction
fromCdl action =
  NoConfidenceAction
    { actionId:
        GovernanceActionId.fromCdl <$>
          toMaybe (Cdl.noConfidenceAction_govActionId action)
    }

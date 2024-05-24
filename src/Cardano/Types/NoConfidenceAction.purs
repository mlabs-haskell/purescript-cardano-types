module Cardano.Types.NoConfidenceAction
  ( NoConfidenceAction(NoConfidenceAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCsl, toCsl) as GovernanceActionId
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
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

toCsl :: NoConfidenceAction -> Csl.NoConfidenceAction
toCsl (NoConfidenceAction { actionId }) =
  maybe Csl.noConfidenceAction_new
    (Csl.noConfidenceAction_newWithActionId <<< GovernanceActionId.toCsl)
    actionId

fromCsl :: Csl.NoConfidenceAction -> NoConfidenceAction
fromCsl action =
  NoConfidenceAction
    { actionId:
        GovernanceActionId.fromCsl <$>
          toMaybe (Csl.noConfidenceAction_govActionId action)
    }

module Cardano.Types.HardForkInitiationAction
  ( HardForkInitiationAction(HardForkInitiationAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCsl, toCsl) as GovernanceActionId
import Cardano.Types.ProtocolVersion (ProtocolVersion)
import Cardano.Types.ProtocolVersion (fromCsl, toCsl) as ProtocolVersion
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

-- Triggers a non-backwards compatible upgrade of the network;
-- requires a prior software upgrade.
newtype HardForkInitiationAction = HardForkInitiationAction
  { protocolVersion :: ProtocolVersion
  , actionId :: Maybe GovernanceActionId
  -- ^ pointer to the previously enacted proposal modifying the
  -- same piece of state
  }

derive instance Generic HardForkInitiationAction _
derive instance Newtype HardForkInitiationAction _
derive instance Eq HardForkInitiationAction
derive instance Ord HardForkInitiationAction
derive newtype instance EncodeAeson HardForkInitiationAction
derive newtype instance DecodeAeson HardForkInitiationAction

instance Show HardForkInitiationAction where
  show = genericShow

toCsl :: HardForkInitiationAction -> Csl.HardForkInitiationAction
toCsl (HardForkInitiationAction rec) =
  case rec.actionId of
    Nothing ->
      Csl.hardForkInitiationAction_new protocolVersion
    Just actionId ->
      Csl.hardForkInitiationAction_newWithActionId (GovernanceActionId.toCsl actionId)
        protocolVersion
  where
  protocolVersion = ProtocolVersion.toCsl rec.protocolVersion

fromCsl :: Csl.HardForkInitiationAction -> HardForkInitiationAction
fromCsl action =
  HardForkInitiationAction
    { protocolVersion:
        ProtocolVersion.fromCsl $
          Csl.hardForkInitiationAction_protocolVersion action
    , actionId:
        GovernanceActionId.fromCsl <$>
          toMaybe (Csl.hardForkInitiationAction_govActionId action)
    }

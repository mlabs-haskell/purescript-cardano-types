module Cardano.Types.HardForkInitiationAction
  ( HardForkInitiationAction(HardForkInitiationAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
import Cardano.Types.ProtocolVersion (ProtocolVersion)
import Cardano.Types.ProtocolVersion (fromCdl, toCdl) as ProtocolVersion
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
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

instance AsCbor HardForkInitiationAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: HardForkInitiationAction -> Cdl.HardForkInitiationAction
toCdl (HardForkInitiationAction rec) =
  case rec.actionId of
    Nothing ->
      Cdl.hardForkInitiationAction_new protocolVersion
    Just actionId ->
      Cdl.hardForkInitiationAction_newWithActionId (GovernanceActionId.toCdl actionId)
        protocolVersion
  where
  protocolVersion = ProtocolVersion.toCdl rec.protocolVersion

fromCdl :: Cdl.HardForkInitiationAction -> HardForkInitiationAction
fromCdl action =
  HardForkInitiationAction
    { protocolVersion:
        ProtocolVersion.fromCdl $
          Cdl.hardForkInitiationAction_protocolVersion action
    , actionId:
        GovernanceActionId.fromCdl <$>
          toMaybe (Cdl.hardForkInitiationAction_govActionId action)
    }

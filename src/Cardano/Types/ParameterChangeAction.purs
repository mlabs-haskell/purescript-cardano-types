module Cardano.Types.ParameterChangeAction
  ( ParameterChangeAction(ParameterChangeAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
import Cardano.Types.ProtocolParamUpdate (ProtocolParamUpdate)
import Cardano.Types.ProtocolParamUpdate (fromCdl, toCdl) as ProtocolParamUpdate
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

-- A change to one or more updatable protocol parameters, excluding
-- changes to major protocol versions (hard forks). 
newtype ParameterChangeAction = ParameterChangeAction
  { pparamsUpdate :: ProtocolParamUpdate
  , actionId :: Maybe GovernanceActionId
  -- ^ pointer to the previously enacted proposal modifying the
  -- same piece of state
  , policyHash :: Maybe ScriptHash
  -- ^ pointer to the proposal policy
  }

derive instance Generic ParameterChangeAction _
derive instance Newtype ParameterChangeAction _
derive instance Eq ParameterChangeAction
derive instance Ord ParameterChangeAction
derive newtype instance EncodeAeson ParameterChangeAction
derive newtype instance DecodeAeson ParameterChangeAction

instance Show ParameterChangeAction where
  show = genericShow

instance AsCbor ParameterChangeAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: ParameterChangeAction -> Cdl.ParameterChangeAction
toCdl (ParameterChangeAction rec) =
  case rec.actionId, rec.policyHash of
    Nothing, Nothing ->
      Cdl.parameterChangeAction_new pparamsUpdate
    Just actionId, Nothing ->
      Cdl.parameterChangeAction_newWithActionId (GovernanceActionId.toCdl actionId) pparamsUpdate
    Nothing, Just policyHash ->
      Cdl.parameterChangeAction_newWithPolicyHash pparamsUpdate (unwrap policyHash)
    Just actionId, Just policyHash ->
      Cdl.parameterChangeAction_newWithPolicyHashAndActionId
        (GovernanceActionId.toCdl actionId)
        pparamsUpdate
        (unwrap policyHash)
  where
  pparamsUpdate = ProtocolParamUpdate.toCdl rec.pparamsUpdate

fromCdl :: Cdl.ParameterChangeAction -> ParameterChangeAction
fromCdl action =
  ParameterChangeAction
    { pparamsUpdate:
        ProtocolParamUpdate.fromCdl $
          Cdl.parameterChangeAction_protocolParamUpdates action
    , actionId:
        GovernanceActionId.fromCdl <$>
          toMaybe (Cdl.parameterChangeAction_govActionId action)
    , policyHash:
        wrap <$> toMaybe (Cdl.parameterChangeAction_policyHash action)
    }

module Cardano.Types.ParameterChangeAction
  ( ParameterChangeAction(ParameterChangeAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCsl, toCsl) as GovernanceActionId
import Cardano.Types.ProtocolParamUpdate (ProtocolParamUpdate)
import Cardano.Types.ProtocolParamUpdate (fromCsl, toCsl) as ProtocolParamUpdate
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
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: ParameterChangeAction -> Csl.ParameterChangeAction
toCsl (ParameterChangeAction rec) =
  case rec.actionId, rec.policyHash of
    Nothing, Nothing ->
      Csl.parameterChangeAction_new pparamsUpdate
    Just actionId, Nothing ->
      Csl.parameterChangeAction_newWithActionId (GovernanceActionId.toCsl actionId) pparamsUpdate
    Nothing, Just policyHash ->
      Csl.parameterChangeAction_newWithPolicyHash pparamsUpdate (unwrap policyHash)
    Just actionId, Just policyHash ->
      Csl.parameterChangeAction_newWithPolicyHashAndActionId
        (GovernanceActionId.toCsl actionId)
        pparamsUpdate
        (unwrap policyHash)
  where
  pparamsUpdate = ProtocolParamUpdate.toCsl rec.pparamsUpdate

fromCsl :: Csl.ParameterChangeAction -> ParameterChangeAction
fromCsl action =
  ParameterChangeAction
    { pparamsUpdate:
        ProtocolParamUpdate.fromCsl $
          Csl.parameterChangeAction_protocolParamUpdates action
    , actionId:
        GovernanceActionId.fromCsl <$>
          toMaybe (Csl.parameterChangeAction_govActionId action)
    , policyHash:
        wrap <$> toMaybe (Csl.parameterChangeAction_policyHash action)
    }

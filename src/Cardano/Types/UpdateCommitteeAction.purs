module Cardano.Types.UpdateCommitteeAction
  ( UpdateCommitteeAction(UpdateCommitteeAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (packListContainer, unpackListContainer)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Committee (Committee)
import Cardano.Types.Committee (fromCdl, toCdl) as Committee
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCdl, toCdl) as Credential
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype UpdateCommitteeAction = UpdateCommitteeAction
  { committee :: Committee
  , membersToRemove :: Array Credential
  , actionId :: Maybe GovernanceActionId
  }

derive instance Generic UpdateCommitteeAction _
derive instance Newtype UpdateCommitteeAction _
derive instance Eq UpdateCommitteeAction
derive instance Ord UpdateCommitteeAction
derive newtype instance EncodeAeson UpdateCommitteeAction
derive newtype instance DecodeAeson UpdateCommitteeAction

instance Show UpdateCommitteeAction where
  show = genericShow

instance AsCbor UpdateCommitteeAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: UpdateCommitteeAction -> Cdl.UpdateCommitteeAction
toCdl (UpdateCommitteeAction rec) =
  case rec.actionId of
    Nothing ->
      Cdl.updateCommitteeAction_new committee membersToRemove
    Just actionId ->
      Cdl.updateCommitteeAction_newWithActionId (GovernanceActionId.toCdl actionId) committee
        membersToRemove
  where
  committee = Committee.toCdl rec.committee
  membersToRemove = packListContainer $ Credential.toCdl <$> rec.membersToRemove

fromCdl :: Cdl.UpdateCommitteeAction -> UpdateCommitteeAction
fromCdl action =
  UpdateCommitteeAction
    { committee:
        Committee.fromCdl $
          Cdl.updateCommitteeAction_committee action
    , membersToRemove:
        Credential.fromCdl <$>
          unpackListContainer (Cdl.updateCommitteeAction_membersToRemove action)
    , actionId:
        GovernanceActionId.fromCdl <$>
          toMaybe (Cdl.updateCommitteeAction_govActionId action)
    }

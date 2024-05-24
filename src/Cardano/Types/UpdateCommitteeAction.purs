module Cardano.Types.UpdateCommitteeAction
  ( UpdateCommitteeAction(UpdateCommitteeAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib (packListContainer, unpackListContainer)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Committee (Committee)
import Cardano.Types.Committee (fromCsl, toCsl) as Committee
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCsl, toCsl) as Credential
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCsl, toCsl) as GovernanceActionId
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
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

toCsl :: UpdateCommitteeAction -> Csl.UpdateCommitteeAction
toCsl (UpdateCommitteeAction rec) =
  case rec.actionId of
    Nothing ->
      Csl.updateCommitteeAction_new committee membersToRemove
    Just actionId ->
      Csl.updateCommitteeAction_newWithActionId (GovernanceActionId.toCsl actionId) committee
        membersToRemove
  where
  committee = Committee.toCsl rec.committee
  membersToRemove = packListContainer $ Credential.toCsl <$> rec.membersToRemove

fromCsl :: Csl.UpdateCommitteeAction -> UpdateCommitteeAction
fromCsl action =
  UpdateCommitteeAction
    { committee:
        Committee.fromCsl $
          Csl.updateCommitteeAction_committee action
    , membersToRemove:
        Credential.fromCsl <$>
          unpackListContainer (Csl.updateCommitteeAction_membersToRemove action)
    , actionId:
        GovernanceActionId.fromCsl <$>
          toMaybe (Csl.updateCommitteeAction_govActionId action)
    }

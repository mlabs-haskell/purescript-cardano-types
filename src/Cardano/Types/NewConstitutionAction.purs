module Cardano.Types.NewConstitutionAction
  ( NewConstitutionAction(NewConstitutionAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.Constitution (Constitution)
import Cardano.Types.Constitution (fromCsl, toCsl) as Constitution
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCsl, toCsl) as GovernanceActionId
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype NewConstitutionAction = NewConstitutionAction
  { constitution :: Constitution
  , actionId :: Maybe GovernanceActionId
  }

derive instance Generic NewConstitutionAction _
derive instance Newtype NewConstitutionAction _
derive instance Eq NewConstitutionAction
derive instance Ord NewConstitutionAction
derive newtype instance EncodeAeson NewConstitutionAction
derive newtype instance DecodeAeson NewConstitutionAction

instance Show NewConstitutionAction where
  show = genericShow

instance AsCbor NewConstitutionAction where
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: NewConstitutionAction -> Csl.NewConstitutionAction
toCsl (NewConstitutionAction rec) =
  maybe (Csl.newConstitutionAction_new constitution)
    (flip Csl.newConstitutionAction_newWithActionId constitution <<< GovernanceActionId.toCsl)
    rec.actionId
  where
  constitution = Constitution.toCsl rec.constitution

fromCsl :: Csl.NewConstitutionAction -> NewConstitutionAction
fromCsl action =
  NewConstitutionAction
    { constitution:
        Constitution.fromCsl $
          Csl.newConstitutionAction_constitution action
    , actionId:
        GovernanceActionId.fromCsl <$>
          toMaybe (Csl.newConstitutionAction_govActionId action)
    }

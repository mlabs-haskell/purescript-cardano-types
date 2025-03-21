module Cardano.Types.NewConstitutionAction
  ( NewConstitutionAction(NewConstitutionAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Constitution (Constitution)
import Cardano.Types.Constitution (fromCdl, toCdl) as Constitution
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
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
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: NewConstitutionAction -> Cdl.NewConstitutionAction
toCdl (NewConstitutionAction rec) =
  maybe (Cdl.newConstitutionAction_new constitution)
    (flip Cdl.newConstitutionAction_newWithActionId constitution <<< GovernanceActionId.toCdl)
    rec.actionId
  where
  constitution = Constitution.toCdl rec.constitution

fromCdl :: Cdl.NewConstitutionAction -> NewConstitutionAction
fromCdl action =
  NewConstitutionAction
    { constitution:
        Constitution.fromCdl $
          Cdl.newConstitutionAction_constitution action
    , actionId:
        GovernanceActionId.fromCdl <$>
          toMaybe (Cdl.newConstitutionAction_govActionId action)
    }

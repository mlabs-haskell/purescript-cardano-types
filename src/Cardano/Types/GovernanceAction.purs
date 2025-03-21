module Cardano.Types.GovernanceAction
  ( GovernanceAction
      ( ChangePParams
      , TriggerHF
      , TreasuryWdrl
      , NoConfidence
      , NewCommittee
      , NewConstitution
      , Info
      )
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.HardForkInitiationAction (HardForkInitiationAction)
import Cardano.Types.HardForkInitiationAction (fromCdl, toCdl) as HardForkInitiationAction
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.NewConstitutionAction (NewConstitutionAction)
import Cardano.Types.NewConstitutionAction (fromCdl, toCdl) as NewConstitutionAction
import Cardano.Types.NoConfidenceAction (NoConfidenceAction)
import Cardano.Types.NoConfidenceAction (fromCdl, toCdl) as NoConfidenceAction
import Cardano.Types.ParameterChangeAction (ParameterChangeAction)
import Cardano.Types.ParameterChangeAction (fromCdl, toCdl) as ParameterChangeAction
import Cardano.Types.TreasuryWithdrawalsAction (TreasuryWithdrawalsAction)
import Cardano.Types.TreasuryWithdrawalsAction (fromCdl, toCdl) as TreasuryWithdrawalsAction
import Cardano.Types.UpdateCommitteeAction (UpdateCommitteeAction)
import Cardano.Types.UpdateCommitteeAction (fromCdl, toCdl) as UpdateCommitteeAction
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data GovernanceAction
  = ChangePParams ParameterChangeAction
  | TriggerHF HardForkInitiationAction
  | TreasuryWdrl TreasuryWithdrawalsAction
  | NoConfidence NoConfidenceAction
  | NewCommittee UpdateCommitteeAction
  | NewConstitution NewConstitutionAction
  | Info

derive instance Generic GovernanceAction _
derive instance Eq GovernanceAction
derive instance Ord GovernanceAction

instance Show GovernanceAction where
  show = genericShow

instance AsCbor GovernanceAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

instance EncodeAeson GovernanceAction where
  encodeAeson = case _ of
    ChangePParams rec -> encodeTagged' "ChangePParams" rec
    TriggerHF rec -> encodeTagged' "TriggerHF" rec
    TreasuryWdrl rec -> encodeTagged' "TreasuryWdrl" rec
    NoConfidence rec -> encodeTagged' "NoConfidence" rec
    NewCommittee rec -> encodeTagged' "NewCommittee" rec
    NewConstitution rec -> encodeTagged' "NewConstitution" rec
    Info -> encodeAeson { tag: "Info" }

instance DecodeAeson GovernanceAction where
  decodeAeson = decodeAeson >=> \obj -> do
    tag <- obj .: "tag"
    let
      aesonContents :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: "contents"
    case tag of
      "ChangePParams" -> ChangePParams <$> aesonContents
      "TriggerHF" -> TriggerHF <$> aesonContents
      "TreasuryWdrl" -> TreasuryWdrl <$> aesonContents
      "NoConfidence" -> NoConfidence <$> aesonContents
      "NewCommittee" -> NewCommittee <$> aesonContents
      "NewConstitution" -> NewConstitution <$> aesonContents
      "Info" -> pure Info
      _ -> Left $ TypeMismatch $ "Unknown tag: " <> tag

toCdl :: GovernanceAction -> Cdl.GovernanceAction
toCdl = case _ of
  ChangePParams action ->
    Cdl.governanceAction_newParameterChangeAction
      (ParameterChangeAction.toCdl action)
  TriggerHF action ->
    Cdl.governanceAction_newHardForkInitiationAction
      (HardForkInitiationAction.toCdl action)
  TreasuryWdrl action ->
    Cdl.governanceAction_newTreasuryWithdrawalsAction
      (TreasuryWithdrawalsAction.toCdl action)
  NoConfidence action ->
    Cdl.governanceAction_newNoConfidenceAction
      (NoConfidenceAction.toCdl action)
  NewCommittee action ->
    Cdl.governanceAction_newNewCommitteeAction
      (UpdateCommitteeAction.toCdl action)
  NewConstitution action ->
    Cdl.governanceAction_newNewConstitutionAction
      (NewConstitutionAction.toCdl action)
  Info ->
    Cdl.governanceAction_newInfoAction
      Cdl.infoAction_new

fromCdl :: Cdl.GovernanceAction -> GovernanceAction
fromCdl action =
  unsafePartial fromJust $
    ( ChangePParams <<< ParameterChangeAction.fromCdl <$>
        toMaybe (Cdl.governanceAction_asParameterChangeAction action)
    )
      <|>
        ( TriggerHF <<< HardForkInitiationAction.fromCdl <$>
            toMaybe (Cdl.governanceAction_asHardForkInitiationAction action)
        )
      <|>
        ( TreasuryWdrl <<< TreasuryWithdrawalsAction.fromCdl <$>
            toMaybe (Cdl.governanceAction_asTreasuryWithdrawalsAction action)
        )
      <|>
        ( NoConfidence <<< NoConfidenceAction.fromCdl <$>
            toMaybe (Cdl.governanceAction_asNoConfidenceAction action)
        )
      <|>
        ( NewCommittee <<< UpdateCommitteeAction.fromCdl <$>
            toMaybe (Cdl.governanceAction_asNewCommitteeAction action)
        )
      <|>
        ( NewConstitution <<< NewConstitutionAction.fromCdl <$>
            toMaybe (Cdl.governanceAction_asNewConstitutionAction action)
        )
      <|>
        ( Info <$
            toMaybe (Cdl.governanceAction_asInfoAction action)
        )

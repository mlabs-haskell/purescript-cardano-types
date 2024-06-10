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
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.HardForkInitiationAction (HardForkInitiationAction)
import Cardano.Types.HardForkInitiationAction (fromCsl, toCsl) as HardForkInitiationAction
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.NewConstitutionAction (NewConstitutionAction)
import Cardano.Types.NewConstitutionAction (fromCsl, toCsl) as NewConstitutionAction
import Cardano.Types.NoConfidenceAction (NoConfidenceAction)
import Cardano.Types.NoConfidenceAction (fromCsl, toCsl) as NoConfidenceAction
import Cardano.Types.ParameterChangeAction (ParameterChangeAction)
import Cardano.Types.ParameterChangeAction (fromCsl, toCsl) as ParameterChangeAction
import Cardano.Types.TreasuryWithdrawalsAction (TreasuryWithdrawalsAction)
import Cardano.Types.TreasuryWithdrawalsAction (fromCsl, toCsl) as TreasuryWithdrawalsAction
import Cardano.Types.UpdateCommitteeAction (UpdateCommitteeAction)
import Cardano.Types.UpdateCommitteeAction (fromCsl, toCsl) as UpdateCommitteeAction
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
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

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

toCsl :: GovernanceAction -> Csl.GovernanceAction
toCsl = case _ of
  ChangePParams action ->
    Csl.governanceAction_newParameterChangeAction
      (ParameterChangeAction.toCsl action)
  TriggerHF action ->
    Csl.governanceAction_newHardForkInitiationAction
      (HardForkInitiationAction.toCsl action)
  TreasuryWdrl action ->
    Csl.governanceAction_newTreasuryWithdrawalsAction
      (TreasuryWithdrawalsAction.toCsl action)
  NoConfidence action ->
    Csl.governanceAction_newNoConfidenceAction
      (NoConfidenceAction.toCsl action)
  NewCommittee action ->
    Csl.governanceAction_newNewCommitteeAction
      (UpdateCommitteeAction.toCsl action)
  NewConstitution action ->
    Csl.governanceAction_newNewConstitutionAction
      (NewConstitutionAction.toCsl action)
  Info ->
    Csl.governanceAction_newInfoAction
      Csl.infoAction_new

fromCsl :: Csl.GovernanceAction -> GovernanceAction
fromCsl action =
  unsafePartial fromJust $
    ( ChangePParams <<< ParameterChangeAction.fromCsl <$>
        toMaybe (Csl.governanceAction_asParameterChangeAction action)
    )
      <|>
        ( TriggerHF <<< HardForkInitiationAction.fromCsl <$>
            toMaybe (Csl.governanceAction_asHardForkInitiationAction action)
        )
      <|>
        ( TreasuryWdrl <<< TreasuryWithdrawalsAction.fromCsl <$>
            toMaybe (Csl.governanceAction_asTreasuryWithdrawalsAction action)
        )
      <|>
        ( NoConfidence <<< NoConfidenceAction.fromCsl <$>
            toMaybe (Csl.governanceAction_asNoConfidenceAction action)
        )
      <|>
        ( NewCommittee <<< UpdateCommitteeAction.fromCsl <$>
            toMaybe (Csl.governanceAction_asNewCommitteeAction action)
        )
      <|>
        ( NewConstitution <<< NewConstitutionAction.fromCsl <$>
            toMaybe (Csl.governanceAction_asNewConstitutionAction action)
        )
      <|>
        ( Info <$
            toMaybe (Csl.governanceAction_asInfoAction action)
        )

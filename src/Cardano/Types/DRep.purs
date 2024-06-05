module Cardano.Types.DRep
  ( DRep(DrepCred, AlwaysAbstain, AlwaysNoConfidence)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson, (.:))
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Credential (Credential(PubKeyHashCredential, ScriptHashCredential))
import Cardano.Types.Internal.Helpers (encodeTagged')
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data DRep
  = DrepCred Credential
  | AlwaysAbstain
  | AlwaysNoConfidence

derive instance Generic DRep _
derive instance Eq DRep
derive instance Ord DRep

instance Show DRep where
  show = genericShow

instance EncodeAeson DRep where
  encodeAeson = case _ of
    DrepCred cred -> encodeTagged' "DrepCred" cred
    AlwaysAbstain -> encodeAeson { tag: "AlwaysAbstain" }
    AlwaysNoConfidence -> encodeAeson { tag: "AlwaysNoConfidence" }

instance DecodeAeson DRep where
  decodeAeson = decodeAeson >=> \obj -> do
    tag <- obj .: "tag"
    case tag of
      "DrepCred" -> DrepCred <$> obj .: "contents"
      "AlwaysAbstain" -> pure AlwaysAbstain
      "AlwaysNoConfidence" -> pure AlwaysNoConfidence
      _ -> Left $ TypeMismatch $ "Unknown tag: " <> tag

toCsl :: DRep -> Csl.DRep
toCsl = case _ of
  DrepCred (PubKeyHashCredential keyHash) -> Csl.dRep_newKeyHash $ unwrap keyHash
  DrepCred (ScriptHashCredential scriptHash) -> Csl.dRep_newScriptHash $ unwrap scriptHash
  AlwaysAbstain -> Csl.dRep_newAlwaysAbstain
  AlwaysNoConfidence -> Csl.dRep_newAlwaysNoConfidence

fromCsl :: Csl.DRep -> DRep
fromCsl drep =
  unsafePartial fromJust $
    case Csl.fromCslEnum (Csl.dRep_kind drep) of
      Csl.DRepKind_KeyHash ->
        DrepCred <<< PubKeyHashCredential <<< wrap <$>
          toMaybe (Csl.dRep_toKeyHash drep)
      Csl.DRepKind_ScriptHash ->
        DrepCred <<< ScriptHashCredential <<< wrap <$>
          toMaybe (Csl.dRep_toScriptHash drep)
      Csl.DRepKind_AlwaysAbstain -> pure AlwaysAbstain
      Csl.DRepKind_AlwaysNoConfidence -> pure AlwaysNoConfidence

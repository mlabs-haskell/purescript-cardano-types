module Cardano.Types.DRep
  ( DRep(DrepCred, AlwaysAbstain, AlwaysNoConfidence)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
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

instance AsCbor DRep where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: DRep -> Cdl.DRep
toCdl = case _ of
  DrepCred (PubKeyHashCredential keyHash) -> Cdl.dRep_newKeyHash $ unwrap keyHash
  DrepCred (ScriptHashCredential scriptHash) -> Cdl.dRep_newScriptHash $ unwrap scriptHash
  AlwaysAbstain -> Cdl.dRep_newAlwaysAbstain
  AlwaysNoConfidence -> Cdl.dRep_newAlwaysNoConfidence

fromCdl :: Cdl.DRep -> DRep
fromCdl drep =
  unsafePartial fromJust $
    case Cdl.fromCslEnum (Cdl.dRep_kind drep) of
      Cdl.DRepKind_KeyHash ->
        DrepCred <<< PubKeyHashCredential <<< wrap <$>
          toMaybe (Cdl.dRep_toKeyHash drep)
      Cdl.DRepKind_ScriptHash ->
        DrepCred <<< ScriptHashCredential <<< wrap <$>
          toMaybe (Cdl.dRep_toScriptHash drep)
      Cdl.DRepKind_AlwaysAbstain -> pure AlwaysAbstain
      Cdl.DRepKind_AlwaysNoConfidence -> pure AlwaysNoConfidence

module Cardano.Types.Voter
  ( Voter(Cc, Drep, Spo)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCdl, toCdl) as Credential
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.Internal.Helpers (encodeTagged')
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data Voter
  = Cc Credential -- Constitutional Committee
  | Drep Credential -- Delegated Representative
  | Spo Ed25519KeyHash -- Stake Pool Operator

derive instance Generic Voter _
derive instance Eq Voter
derive instance Ord Voter

instance Show Voter where
  show = genericShow

instance EncodeAeson Voter where
  encodeAeson = case _ of
    Cc rec -> encodeTagged' "Cc" rec
    Drep rec -> encodeTagged' "Drep" rec
    Spo rec -> encodeTagged' "Spo" rec

instance DecodeAeson Voter where
  decodeAeson = decodeAeson >=> \obj -> do
    tag <- obj .: "tag"
    let
      aesonContents :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: "contents"
    case tag of
      "Cc" -> Cc <$> aesonContents
      "Drep" -> Drep <$> aesonContents
      "Spo" -> Spo <$> aesonContents
      _ -> Left $ TypeMismatch $ "Unknown tag: " <> tag

instance AsCbor Voter where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: Voter -> Cdl.Voter
toCdl = case _ of
  Cc cred -> Cdl.voter_newConstitutionalCommitteeHotCredential $ Credential.toCdl cred
  Drep cred -> Cdl.voter_newDrepCredential $ Credential.toCdl cred
  Spo keyHash -> Cdl.voter_newStakePoolKeyHash $ unwrap keyHash

fromCdl :: Cdl.Voter -> Voter
fromCdl voter =
  unsafePartial fromJust $
    ( Cc <<< Credential.fromCdl <$>
        toMaybe (Cdl.voter_toConstitutionalCommitteeHotCredential voter)
    )
      <|>
        ( Drep <<< Credential.fromCdl <$>
            toMaybe (Cdl.voter_toDrepCredential voter)
        )
      <|>
        ( Spo <<< wrap <$>
            toMaybe (Cdl.voter_toStakePoolKeyHash voter)
        )

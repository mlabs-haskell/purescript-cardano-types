module Cardano.Types.Voter
  ( Voter(Cc, Drep, Spo)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCsl, toCsl) as Credential
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
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: Voter -> Csl.Voter
toCsl = case _ of
  Cc cred -> Csl.voter_newConstitutionalCommitteeHotCredential $ Credential.toCsl cred
  Drep cred -> Csl.voter_newDrepCredential $ Credential.toCsl cred
  Spo keyHash -> Csl.voter_newStakePoolKeyHash $ unwrap keyHash

fromCsl :: Csl.Voter -> Voter
fromCsl voter =
  unsafePartial fromJust $
    ( Cc <<< Credential.fromCsl <$>
        toMaybe (Csl.voter_toConstitutionalCommitteeHotCredential voter)
    )
      <|>
        ( Drep <<< Credential.fromCsl <$>
            toMaybe (Csl.voter_toDrepCredential voter)
        )
      <|>
        ( Spo <<< wrap <$>
            toMaybe (Csl.voter_toStakePoolKeyHash voter)
        )

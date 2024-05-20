module Cardano.Types.StakeCredential
  ( StakeCredential(StakeCredential)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(Left, Right))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

newtype StakeCredential = StakeCredential Credential

derive instance Newtype StakeCredential _
derive instance Generic StakeCredential _

instance AsCbor StakeCredential where
  encodeCbor = unwrap >>> encodeCbor
  decodeCbor = decodeCbor >>> map wrap

instance Eq StakeCredential where
  eq = eq `on` encodeCbor

instance Ord StakeCredential where
  compare = compare `on` encodeCbor

instance Show StakeCredential where
  show = genericShow

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson StakeCredential where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded StakeCredential") Right <<<
      caseAesonString Nothing (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson StakeCredential where
  encodeAeson sh = encodeAeson $ encodeCbor sh

instance Arbitrary StakeCredential where
  arbitrary = genericArbitrary

fromCsl :: Csl.Credential -> StakeCredential
fromCsl cslCred =
  case toMaybe (Csl.credential_toKeyhash cslCred) of
    Just hash -> StakeCredential $ PubKeyHashCredential (wrap hash)
    Nothing -> case toMaybe (Csl.credential_toScripthash cslCred) of
      Just hash -> StakeCredential $ ScriptHashCredential (wrap hash)
      Nothing -> unsafePerformEffect $ throw "Cardano.Types.StakeCredential.fromCsl: unknown kind"

toCsl :: StakeCredential -> Csl.Credential
toCsl (StakeCredential cred) =
  case cred of
    PubKeyHashCredential hash ->
      Csl.credential_fromKeyhash (unwrap hash)
    ScriptHashCredential hash ->
      Csl.credential_fromScripthash (unwrap hash)

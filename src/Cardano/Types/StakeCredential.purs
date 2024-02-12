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
import Cardano.Types.Credential (Credential(PubKeyHashCredential, ScriptHashCredential))
import Cardano.Serialization.Lib as Csl
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

fromCsl :: Csl.StakeCredential -> StakeCredential
fromCsl cslc = case toMaybe (Csl.stakeCredential_toKeyhash cslc) of
  Just hash -> StakeCredential $ PubKeyHashCredential (wrap hash)
  Nothing -> case toMaybe (Csl.stakeCredential_toScripthash cslc) of
    Just hash -> StakeCredential $ ScriptHashCredential (wrap hash)
    Nothing -> unsafePerformEffect $ throw "Cardano.Types.StakeCredential.fromCsl: unknown kind"

toCsl :: StakeCredential -> Csl.StakeCredential
toCsl (StakeCredential (PubKeyHashCredential hash)) = Csl.stakeCredential_fromKeyhash (unwrap hash)
toCsl (StakeCredential (ScriptHashCredential hash)) = Csl.stakeCredential_fromScripthash (unwrap hash)

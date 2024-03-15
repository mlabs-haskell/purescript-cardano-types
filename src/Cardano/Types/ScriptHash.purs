module Cardano.Types.ScriptHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.Serialization.Lib
  ( fromBytes
  , scriptHash_fromBech32
  , scriptHash_toBech32
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.TransactionMetadatum (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray (byteArrayFromIntArrayUnsafe, hexToByteArray)
import Data.Either (Either(Right, Left))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

-- | blake2b-224 hash digests of serialized monetary scripts
newtype ScriptHash = ScriptHash Csl.ScriptHash

derive instance Newtype ScriptHash _
derive instance Generic ScriptHash _

instance Eq ScriptHash where
  eq = eq `on` encodeCbor

instance Ord ScriptHash where
  compare = compare `on` encodeCbor

instance Show ScriptHash where
  show = genericShow

instance Arbitrary ScriptHash where
  arbitrary =
    unsafePartial $
      fromJust <<< decodeCbor <<< wrap <<< byteArrayFromIntArrayUnsafe
        <$> vectorOf 28 (chooseInt 0 255)

instance AsCbor ScriptHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData ScriptHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData ScriptHash where
  fromData (Bytes bytes) = decodeCbor $ wrap bytes
  fromData _ = Nothing

instance ToMetadata ScriptHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata ScriptHash where
  fromMetadata (Metadata.Bytes bytes) = decodeCbor $ wrap bytes
  fromMetadata _ = Nothing

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson ScriptHash where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded script hash") Right <<<
      caseAesonString Nothing
        (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson ScriptHash where
  encodeAeson sh = encodeAeson $ encodeCbor sh

-- | Decodes a script hash from its Bech32 representation
fromBech32 :: Bech32String -> Maybe ScriptHash
fromBech32 = map wrap <<< toMaybe <<< scriptHash_fromBech32

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will fail if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
toBech32Unsafe :: Partial => String -> ScriptHash -> Bech32String
toBech32Unsafe prefix = unwrap >>> flip scriptHash_toBech32 prefix

module Cardano.Types.Ed25519KeyHash where

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
  ( ed25519KeyHash_fromBech32
  , ed25519KeyHash_toBech32
  , fromBytes
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.TransactionMetadatum
  ( TransactionMetadatum(Bytes)
  ) as Metadata
import Data.ByteArray
  ( byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , hexToByteArray
  )
import Data.Either (Either(Left), hush, note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Effect.Exception (try)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype Ed25519KeyHash = Ed25519KeyHash Csl.Ed25519KeyHash

derive instance Generic Ed25519KeyHash _
derive instance Newtype Ed25519KeyHash _

instance Eq Ed25519KeyHash where
  eq = eq `on` encodeCbor

instance Ord Ed25519KeyHash where
  compare = compare `on` encodeCbor

instance Show Ed25519KeyHash where
  show edkh =
    "(Ed25519KeyHash $ unsafePartial $ fromJust $ ed25519KeyHashFromBech32 "
      <> show (toBech32 "pool" edkh)
      <> ")"

instance AsCbor Ed25519KeyHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData Ed25519KeyHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData Ed25519KeyHash where
  fromData (Bytes kh) = decodeCbor $ wrap kh
  fromData _ = Nothing

instance ToMetadata Ed25519KeyHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata Ed25519KeyHash where
  fromMetadata (Metadata.Bytes kh) = decodeCbor $ wrap kh
  fromMetadata _ = Nothing

-- This is needed for `ApplyArgs`.
instance DecodeAeson Ed25519KeyHash where
  -- ed25519KeyHashFromBech32 goes from Bech32String directly although this
  -- feels unsafe.
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected Plutus BuiltinByteString")
    ( note (TypeMismatch "Invalid Ed25519KeyHash") <<< decodeCbor
        <=< note (TypeMismatch "Invalid ByteArray") <<< map wrap <<< hexToByteArray
    )

instance EncodeAeson Ed25519KeyHash where
  encodeAeson = encodeAeson <<< byteArrayToHex <<< unwrap <<< encodeCbor

instance Arbitrary Ed25519KeyHash where
  arbitrary =
    unsafePartial fromJust <<< decodeCbor <<< wrap <<<
      byteArrayFromIntArrayUnsafe <$> vectorOf 28 (chooseInt 0 255)

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
toBech32Unsafe ∷ Partial => String -> Ed25519KeyHash -> Bech32String
toBech32Unsafe prefix kh = ed25519KeyHash_toBech32 (unwrap kh)
  prefix

fromBech32 :: Bech32String -> Maybe Ed25519KeyHash
fromBech32 = map wrap <<< toMaybe <<< ed25519KeyHash_fromBech32

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
toBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
toBech32 prefix kh =
  hush $ unsafePerformEffect $ try $ unsafePartial
    $ pure
    $ toBech32Unsafe prefix kh

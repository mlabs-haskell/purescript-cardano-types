module Cardano.Types.GenesisHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.TransactionMetadatum (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray (byteArrayFromIntArrayUnsafe, hexToByteArray)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype GenesisHash = GenesisHash Csl.GenesisHash

derive instance Newtype GenesisHash _
derive instance Generic GenesisHash _

instance Eq GenesisHash where
  eq = eq `on` encodeCbor

instance Ord GenesisHash where
  compare = compare `on` encodeCbor

instance Show GenesisHash where
  show = genericShow

instance Arbitrary GenesisHash where
  arbitrary =
    unsafePartial $
      fromJust <<< decodeCbor <<< wrap <<< byteArrayFromIntArrayUnsafe
        <$> vectorOf 28 (chooseInt 0 255)

instance AsCbor GenesisHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData GenesisHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData GenesisHash where
  fromData (Bytes bytes) = decodeCbor $ wrap bytes
  fromData _ = Nothing

instance ToMetadata GenesisHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata GenesisHash where
  fromMetadata (Metadata.Bytes bytes) = decodeCbor $ wrap bytes
  fromMetadata _ = Nothing

instance DecodeAeson GenesisHash where
  decodeAeson = do
    decodeAeson >=>
      (note (TypeMismatch "GenesisHash") <<< (decodeCbor <=< map wrap <<< hexToByteArray))

instance EncodeAeson GenesisHash where
  encodeAeson sh = encodeAeson $ encodeCbor sh

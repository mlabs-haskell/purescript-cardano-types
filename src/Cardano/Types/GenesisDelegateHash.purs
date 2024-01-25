module Cardano.Types.GenesisDelegateHash where

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
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
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

newtype GenesisDelegateHash = GenesisDelegateHash Csl.GenesisDelegateHash

derive instance Newtype GenesisDelegateHash _
derive instance Generic GenesisDelegateHash _

instance Eq GenesisDelegateHash where
  eq = eq `on` encodeCbor

instance Ord GenesisDelegateHash where
  compare = compare `on` encodeCbor

instance Show GenesisDelegateHash where
  show = genericShow

instance Arbitrary GenesisDelegateHash where
  arbitrary =
    unsafePartial $
      fromJust <<< decodeCbor <<< wrap <<< byteArrayFromIntArrayUnsafe
        <$> vectorOf 28 (chooseInt 0 255)

instance AsCbor GenesisDelegateHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData GenesisDelegateHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData GenesisDelegateHash where
  fromData (Bytes bytes) = decodeCbor $ wrap bytes
  fromData _ = Nothing

instance ToMetadata GenesisDelegateHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata GenesisDelegateHash where
  fromMetadata (Metadata.Bytes bytes) = decodeCbor $ wrap bytes
  fromMetadata _ = Nothing

instance DecodeAeson GenesisDelegateHash where
  decodeAeson = do
    decodeAeson >=>
      (note (TypeMismatch "GenesisDelegateHash") <<< (decodeCbor <=< map wrap <<< hexToByteArray))

instance EncodeAeson GenesisDelegateHash where
  encodeAeson sh = encodeAeson $ encodeCbor sh

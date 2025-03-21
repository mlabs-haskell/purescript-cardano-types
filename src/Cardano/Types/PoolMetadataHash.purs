module Cardano.Types.PoolMetadataHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.TransactionMetadatum (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray (hexToByteArray)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PoolMetadataHash = PoolMetadataHash Cdl.PoolMetadataHash

derive instance Newtype PoolMetadataHash _
derive instance Generic PoolMetadataHash _

instance Eq PoolMetadataHash where
  eq = eq `on` encodeCbor

instance Ord PoolMetadataHash where
  compare = compare `on` encodeCbor

instance Show PoolMetadataHash where
  show = genericShow

instance AsCbor PoolMetadataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData PoolMetadataHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData PoolMetadataHash where
  fromData (Bytes bytes) = decodeCbor $ wrap bytes
  fromData _ = Nothing

instance ToMetadata PoolMetadataHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata PoolMetadataHash where
  fromMetadata (Metadata.Bytes bytes) = decodeCbor $ wrap bytes
  fromMetadata _ = Nothing

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson PoolMetadataHash where
  decodeAeson = decodeAeson >=>
    (note (TypeMismatch "PoolMetadataHash") <<< (decodeCbor <=< map wrap <<< hexToByteArray))

instance EncodeAeson PoolMetadataHash where
  encodeAeson sh = encodeAeson $ encodeCbor sh

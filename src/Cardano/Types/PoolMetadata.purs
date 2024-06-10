module Cardano.Types.PoolMetadata
  ( PoolMetadata(PoolMetadata)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (poolMetadata_new, poolMetadata_poolMetadataHash, poolMetadata_url)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.PoolMetadataHash (PoolMetadataHash)
import Cardano.Types.URL (URL)
import Cardano.Types.URL (fromCsl, toCsl) as URL
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PoolMetadata = PoolMetadata
  { url :: URL
  , hash :: PoolMetadataHash
  }

derive instance Generic PoolMetadata _
derive instance Newtype PoolMetadata _
derive instance Eq PoolMetadata
derive instance Ord PoolMetadata
derive newtype instance EncodeAeson PoolMetadata
derive newtype instance DecodeAeson PoolMetadata

instance Show PoolMetadata where
  show = genericShow

instance AsCbor PoolMetadata where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.PoolMetadata -> PoolMetadata
fromCsl csl = PoolMetadata
  { url: URL.fromCsl $ poolMetadata_url csl
  , hash: wrap $ poolMetadata_poolMetadataHash csl
  }

toCsl :: PoolMetadata -> Csl.PoolMetadata
toCsl (PoolMetadata { url, hash }) = poolMetadata_new (URL.toCsl url) (unwrap hash)

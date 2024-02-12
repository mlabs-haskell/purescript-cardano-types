module Cardano.Types.PoolMetadata where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib
  ( poolMetadata_new
  , poolMetadata_poolMetadataHash
  , poolMetadata_url
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.PoolMetadataHash (PoolMetadataHash)
import Cardano.Types.URL (URL)
import Cardano.Types.URL as URL
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PoolMetadata = PoolMetadata
  { url :: URL
  , hash :: PoolMetadataHash
  }

derive instance Eq PoolMetadata
derive instance Ord PoolMetadata
derive instance Generic PoolMetadata _
derive newtype instance EncodeAeson PoolMetadata
derive newtype instance DecodeAeson PoolMetadata

instance Show PoolMetadata where
  show = genericShow

fromCsl :: Csl.PoolMetadata -> PoolMetadata
fromCsl csl = PoolMetadata
  { url: URL.fromCsl $ poolMetadata_url csl
  , hash: wrap $ poolMetadata_poolMetadataHash csl
  }

toCsl :: PoolMetadata -> Csl.PoolMetadata
toCsl (PoolMetadata { url, hash }) = poolMetadata_new (URL.toCsl url) (unwrap hash)

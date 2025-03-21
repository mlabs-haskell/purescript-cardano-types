module Cardano.Types.PoolMetadata
  ( PoolMetadata(PoolMetadata)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (poolMetadata_new, poolMetadata_poolMetadataHash, poolMetadata_url)
import Cardano.Data.Lite as Cdl
import Cardano.Types.PoolMetadataHash (PoolMetadataHash)
import Cardano.Types.URL (URL)
import Cardano.Types.URL (fromCdl, toCdl) as URL
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
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.PoolMetadata -> PoolMetadata
fromCdl csl = PoolMetadata
  { url: URL.fromCdl $ poolMetadata_url csl
  , hash: wrap $ poolMetadata_poolMetadataHash csl
  }

toCdl :: PoolMetadata -> Cdl.PoolMetadata
toCdl (PoolMetadata { url, hash }) = poolMetadata_new (URL.toCdl url) (unwrap hash)

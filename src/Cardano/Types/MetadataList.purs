module Cardano.Types.MetadataList
  ( MetadataList(..)
  , newMetadataList
  )
  where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)

-- TODO This has -> instance IsListContainer Csl.MetadataList TransactionMetadatum
-- So maybe it should be constructed in a different way such as Array of TransactionMetadatum
newtype MetadataList = MetadataList Csl.MetadataList

newMetadataList :: Effect MetadataList
newMetadataList = MetadataList <$> Csl.metadataList_new

derive instance Newtype MetadataList _
derive instance Generic MetadataList _

instance Eq MetadataList where
  eq = eqOrd

instance Ord MetadataList where
  compare = compareViaCslBytes `on` unwrap

instance AsCbor MetadataList where
  encodeCbor = unwrap >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map wrap

instance Show MetadataList where
  show = genericShow

module Cardano.Types.GeneralTransactionMetadata where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainer)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Internal.Helpers (encodeMap)
import Cardano.Types.TransactionMetadatum (TransactionMetadatum)
import Cardano.Types.TransactionMetadatum as TransactionMetadatum
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata
    (Map BigNum TransactionMetadatum)

derive instance Newtype GeneralTransactionMetadata _

derive newtype instance Eq GeneralTransactionMetadata
derive instance Generic GeneralTransactionMetadata _

instance Show GeneralTransactionMetadata where
  show = genericShow

instance EncodeAeson GeneralTransactionMetadata where
  encodeAeson (GeneralTransactionMetadata m) = encodeMap m

-- This Semigroup instance simply takes the Last value for duplicate keys
-- to avoid a Semigroup instance for TransactionMetadatum.
-- Do we want to avoid a Semigroup instance for TransactionMetadatum? Recursion
-- is fine but how to combine Text with Bytes for example? One would have to take
-- precedence and replace the other.
instance Semigroup GeneralTransactionMetadata where
  append (GeneralTransactionMetadata hm) (GeneralTransactionMetadata hm') =
    GeneralTransactionMetadata $ hm `Map.unionWith (flip const)` hm'

instance Monoid GeneralTransactionMetadata where
  mempty = GeneralTransactionMetadata Map.empty

instance AsCbor GeneralTransactionMetadata where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: GeneralTransactionMetadata -> Csl.GeneralTransactionMetadata
toCsl = unwrap >>> Map.toUnfoldable >>> map (unwrap *** TransactionMetadatum.toCsl) >>> packMapContainer

fromCsl :: Csl.GeneralTransactionMetadata -> GeneralTransactionMetadata
fromCsl = unpackMapContainer >>> map (wrap *** TransactionMetadatum.fromCsl) >>> Map.fromFoldable >>> wrap

module Cardano.Types.GeneralTransactionMetadata where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainer)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Internal.Helpers (decodeMap, encodeMap)
import Cardano.Types.TransactionMetadatum (TransactionMetadatum)
import Cardano.Types.TransactionMetadatum as TransactionMetadatum
import Data.Array (foldr)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata
    (Map BigNum TransactionMetadatum)

derive instance Newtype GeneralTransactionMetadata _

derive newtype instance Eq GeneralTransactionMetadata
derive newtype instance Ord GeneralTransactionMetadata
derive instance Generic GeneralTransactionMetadata _

instance Show GeneralTransactionMetadata where
  show = genericShow

instance EncodeAeson GeneralTransactionMetadata where
  encodeAeson (GeneralTransactionMetadata m) = encodeMap m

instance DecodeAeson GeneralTransactionMetadata where
  decodeAeson = map wrap <<< decodeMap

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

empty :: GeneralTransactionMetadata
empty = wrap Map.empty

-- |  Join together an array of metadata maps without overriding the keys
fold :: Array GeneralTransactionMetadata -> Maybe GeneralTransactionMetadata
fold metas =
  map wrap
    $ foldr consume (Just Map.empty)
    $ join
    $ map (Map.toUnfoldable <<< unwrap) metas
  where
  consume (k /\ v) acc
    | Just oldMap <- acc
    , Nothing <- Map.lookup k oldMap = pure $ Map.insert k v oldMap
    | otherwise = Nothing

toCsl :: GeneralTransactionMetadata -> Csl.GeneralTransactionMetadata
toCsl = unwrap >>> Map.toUnfoldable >>> map (unwrap *** TransactionMetadatum.toCsl) >>> packMapContainer

fromCsl :: Csl.GeneralTransactionMetadata -> GeneralTransactionMetadata
fromCsl = unpackMapContainer >>> map (wrap *** TransactionMetadatum.fromCsl) >>> Map.fromFoldable >>> wrap

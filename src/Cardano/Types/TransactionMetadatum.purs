module Cardano.Types.TransactionMetadatum where

import Prelude

import Control.Alt ((<|>))
import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( packMapContainer
  , transactionMetadatum_asBytes
  , transactionMetadatum_asInt
  , transactionMetadatum_asList
  , transactionMetadatum_asMap
  , transactionMetadatum_asText
  , transactionMetadatum_newBytes
  , transactionMetadatum_newInt
  , transactionMetadatum_newList
  , transactionMetadatum_newMap
  , transactionMetadatum_newText
  , unpackListContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Serialization.Lib.Internal (packListContainer)
import Cardano.Types.Int (Int) as Int
import Cardano.Types.Internal.Helpers (encodeMap, encodeTagged')
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data TransactionMetadatum
  = Map (Map TransactionMetadatum TransactionMetadatum)
  | List (Array TransactionMetadatum)
  | Int Int.Int
  | Bytes ByteArray
  | Text String

derive instance Eq TransactionMetadatum
derive instance Ord TransactionMetadatum
derive instance Generic TransactionMetadatum _

instance Show TransactionMetadatum where
  show x = genericShow x

instance EncodeAeson TransactionMetadatum where
  encodeAeson = case _ of
    Map m -> encodeTagged' "Map" $ encodeMap m
    List arr -> encodeTagged' "List" arr
    Int n -> encodeTagged' "Int" n
    Bytes bytes -> encodeTagged' "Bytes" bytes
    Text string -> encodeTagged' "Text" string

instance AsCbor TransactionMetadatum where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl
  :: TransactionMetadatum -> Csl.TransactionMetadatum
toCsl = case _ of
  Map mp ->
    transactionMetadatum_newMap $ packMapContainer $ map (toCsl *** toCsl) $
      Map.toUnfoldable mp
  List l ->
    transactionMetadatum_newList $ packListContainer $ map toCsl l
  Int int -> transactionMetadatum_newInt $ unwrap int
  Bytes bytes -> transactionMetadatum_newBytes bytes
  Text text -> transactionMetadatum_newText text

fromCsl :: Csl.TransactionMetadatum -> TransactionMetadatum
fromCsl csl = unsafePartial $ fromJust $
  Map <<< unpackMapContainerToMapWith fromCsl fromCsl <$> toMaybe (transactionMetadatum_asMap csl)
    <|> List <<< map fromCsl <<< unpackListContainer <$> toMaybe (transactionMetadatum_asList csl)
    <|> Int <<< wrap <$> toMaybe (transactionMetadatum_asInt csl)
    <|> Bytes <$> toMaybe (transactionMetadatum_asBytes csl)
    <|>
      Text <$> toMaybe (transactionMetadatum_asText csl)

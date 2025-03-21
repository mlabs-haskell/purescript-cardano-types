module Cardano.Types.TransactionMetadatum where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(UnexpectedValue, AtKey, Named)
  , decodeAeson
  , fromString
  , toStringifiedNumbersJson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
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
import Cardano.Data.Lite as Cdl
import Cardano.Data.Lite.Internal (packListContainer)
import Cardano.Types.Int (Int) as Int
import Cardano.Types.Internal.Helpers (decodeMap, encodeMap, encodeTagged')
import Control.Alt ((<|>))
import Data.ByteArray (ByteArray)
import Data.Either (Either(Left))
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

instance DecodeAeson TransactionMetadatum where
  decodeAeson aeson = do
    { tag, contents } <-
      ( decodeAeson aeson :: Either _ { tag :: String, contents :: Aeson }
      )
    case tag of
      "Map" -> Map <$> decodeMap contents
      "List" -> List <$> decodeAeson contents
      "Int" -> Int <$> decodeAeson contents
      "Bytes" -> Bytes <$> decodeAeson contents
      "Text" -> Text <$> decodeAeson contents
      tagValue -> Left $ Named "TransactionMetadatum" $ AtKey "tag" $ UnexpectedValue
        $ toStringifiedNumbersJson
        $ fromString tagValue

instance AsCbor TransactionMetadatum where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

toCdl
  :: TransactionMetadatum -> Cdl.TransactionMetadatum
toCdl = case _ of
  Map mp ->
    transactionMetadatum_newMap $ packMapContainer $ map (toCdl *** toCdl) $
      Map.toUnfoldable mp
  List l ->
    transactionMetadatum_newList $ packListContainer $ map toCdl l
  Int int -> transactionMetadatum_newInt $ unwrap int
  Bytes bytes -> transactionMetadatum_newBytes bytes
  Text text -> transactionMetadatum_newText text

fromCdl :: Cdl.TransactionMetadatum -> TransactionMetadatum
fromCdl csl = unsafePartial $ fromJust $
  Map <<< unpackMapContainerToMapWith fromCdl fromCdl <$> toMaybe (transactionMetadatum_asMap csl)
    <|> List <<< map fromCdl <<< unpackListContainer <$> toMaybe (transactionMetadatum_asList csl)
    <|> Int <<< wrap <$> toMaybe (transactionMetadatum_asInt csl)
    <|> Bytes <$> toMaybe (transactionMetadatum_asBytes csl)
    <|>
      Text <$> toMaybe (transactionMetadatum_asText csl)

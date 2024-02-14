module Cardano.Types.Internal.Helpers
  ( eqOrd
  , showFromBytes
  , showFromCbor
  , compareViaCslBytes
  , decodeMap
  , encodeTagged
  , encodeTagged'
  , encodeMap
  , decodeUtf8
  , withNonEmptyArray
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class DecodeTupleAux
  , class EncodeAeson
  , Aeson
  , JsonDecodeError
  , decodeAeson
  , encodeAeson
  )
import Aeson as Aeson
import Cardano.Serialization.Lib (class IsBytes, packListContainer, toBytes)
import Cardano.Serialization.Lib.Internal (class IsCsl, class IsListContainer)
import Control.Alt ((<|>))
import Data.Bifunctor (bimap)
import Data.Bitraversable (ltraverse)
import Data.ByteArray (ByteArray, byteArrayToHex)
import Data.Either (Either(Left, Right))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (Error)
import Foreign.Object (Object)
import Foreign.Object as Obj

eqOrd :: forall a. Ord a => a -> a -> Boolean
eqOrd a b = compare a b == EQ

compareViaCslBytes
  :: forall a b
   . IsCsl a
  => IsBytes a
  => IsCsl b
  => IsBytes b
  => a
  -> b
  -> Ordering
compareViaCslBytes a b =
  compare (byteArrayToHex $ toBytes a) (byteArrayToHex $ toBytes b)

showFromBytes :: forall a. IsCsl a => IsBytes a => String -> a -> String
showFromBytes typeName a = "(" <> typeName
  <> " $ unsafePartial $ fromJust $ fromBytes "
  <> show (toBytes a)
  <> ")"

showFromCbor :: forall a. IsCsl a => IsBytes a => String -> a -> String
showFromCbor typeName a = "(" <> typeName
  <> " $ unsafePartial $ fromJust $ decodeCbor $ CborBytes $ "
  <> show (toBytes a)
  <> ")"

-- | If `k` is encoded as string, `encodeMap` encodes `Map` as `Object`,
-- | else as an `Array` of `Aeson /\ Aeson` pairs
encodeMap
  :: forall (k :: Type) (v :: Type)
   . EncodeAeson k
  => EncodeAeson v
  => Map k v
  -> Aeson
encodeMap m =
  case traverse (ltraverse Aeson.toString) pairs of
    Just pairs' -> encodeAeson $ Obj.fromFoldable pairs'
    Nothing -> encodeAeson pairs
  where
  pairs :: Array (Aeson /\ Aeson)
  pairs = map (bimap encodeAeson encodeAeson) $ Map.toUnfoldable m

-- TODO: test with encodeMap
decodeMap
  :: forall (k :: Type) (v :: Type)
   . DecodeAeson k
  => Ord k
  => DecodeAeson v
  => DecodeTupleAux (k /\ v)
  => Aeson
  -> Either JsonDecodeError (Map k v)
decodeMap aeson = do
  decodeAsArray <|> decodeAsObject
  where
  decodeAsObject = do
    props <- (decodeAeson aeson :: Either _ (Object v))
    Map.fromFoldable <$> for (Obj.toUnfoldable props :: Array (String /\ v)) \(kString /\ v) -> do
      k <- decodeAeson (encodeAeson kString)
      pure $ k /\ v
  decodeAsArray = do
    Map.fromFoldable <$> (decodeAeson aeson :: Either _ (Array (k /\ v)))

-- | Args: tag value encoder
-- | Encodes `value` using `encoder` as `{ "tag": *encoded tag*, "contents": *encoded value* }`
encodeTagged :: forall a. String -> a -> (a -> Aeson) -> Aeson
encodeTagged tag a encoder =
  encodeAeson $ Obj.fromFoldable
    [ "tag" /\ encodeAeson tag
    , "contents" /\ encoder a
    ]

-- | A wrapper around `encodeTagged` function that uses
-- | `encodeAeson` for encoding the passed value
encodeTagged' :: forall (a :: Type). EncodeAeson a => String -> a -> Aeson
encodeTagged' str x = encodeTagged str x encodeAeson

decodeUtf8 :: ByteArray -> Either Error String
decodeUtf8 ba = _decodeUtf8 ba Left Right

foreign import _decodeUtf8
  :: forall (r :: Type). ByteArray -> (Error -> r) -> (String -> r) -> r

withNonEmptyArray
  :: forall e c
   . IsCsl c
  => IsListContainer c e
  => Array e
  -> (c -> Effect Unit)
  -> Effect Unit
withNonEmptyArray [] _ = pure unit
withNonEmptyArray els f = f $ packListContainer els

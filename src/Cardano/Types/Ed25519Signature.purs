module Cardano.Types.Ed25519Signature where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (ed25519Signature_toBech32, fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype Ed25519Signature = Ed25519Signature Csl.Ed25519Signature

derive instance Generic Ed25519Signature _
derive instance Newtype Ed25519Signature _

instance Eq Ed25519Signature where
  eq a b = compare a b == EQ

instance Ord Ed25519Signature where
  compare = compare `on` (unwrap >>> toBytes)

instance EncodeAeson Ed25519Signature where
  encodeAeson = unwrap >>> toBytes >>> encodeAeson

instance DecodeAeson Ed25519Signature where
  decodeAeson = decodeAeson >=>
    note (TypeMismatch "Ed25519Signature") <<< map wrap <<< fromBytes

instance Show Ed25519Signature where
  show sig = "(Ed25519Signature "
    <> show (ed25519Signature_toBech32 <<< unwrap $ sig)
    <> ")"

instance AsCbor Ed25519Signature where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

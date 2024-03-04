module Cardano.Types.PoolPubKeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.Ed25519KeyHash
  ( fromBech32
  , toBech32Unsafe
  ) as Ed25519KeyHash
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype PoolPubKeyHash = PoolPubKeyHash Ed25519KeyHash

derive instance Newtype PoolPubKeyHash _
derive instance Eq PoolPubKeyHash
derive instance Ord PoolPubKeyHash
derive instance Generic PoolPubKeyHash _
derive newtype instance ToData PoolPubKeyHash
derive newtype instance FromData PoolPubKeyHash
derive newtype instance AsCbor PoolPubKeyHash

instance EncodeAeson PoolPubKeyHash where
  encodeAeson = encodeAeson <<< toBech32

instance DecodeAeson PoolPubKeyHash where
  decodeAeson aeson = do
    str <- decodeAeson aeson
    PoolPubKeyHash <$> note (TypeMismatch "PoolPubKeyHash")
      (Ed25519KeyHash.fromBech32 str)

instance Show PoolPubKeyHash where
  show = genericShow

toBech32 :: PoolPubKeyHash -> Bech32String
toBech32 = unsafePartial $ unwrap >>> Ed25519KeyHash.toBech32Unsafe "pool"

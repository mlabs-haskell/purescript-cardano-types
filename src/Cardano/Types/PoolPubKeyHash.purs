module Cardano.Types.PoolPubKeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types.Ed25519KeyHash
  ( Ed25519KeyHash(Ed25519KeyHash)
  , ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32
  )
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype PoolPubKeyHash = PoolPubKeyHash Ed25519KeyHash

derive instance Newtype PoolPubKeyHash _
derive instance Eq PoolPubKeyHash
derive instance Ord PoolPubKeyHash
derive instance Generic PoolPubKeyHash _
derive newtype instance ToData PoolPubKeyHash
derive newtype instance FromData PoolPubKeyHash

instance EncodeAeson PoolPubKeyHash where
  encodeAeson (PoolPubKeyHash kh) =
    encodeAeson (ed25519KeyHashToBech32 "pool" kh)

instance DecodeAeson PoolPubKeyHash where
  decodeAeson aeson = do
    str <- decodeAeson aeson
    PoolPubKeyHash <$> note (TypeMismatch "PoolPubKeyHash")
      (ed25519KeyHashFromBech32 str)

instance Show PoolPubKeyHash where
  show = genericShow

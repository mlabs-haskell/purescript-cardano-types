module Cardano.Types.Nonce where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Control.Bind (map)
import Data.ByteArray (ByteArray)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)


data Nonce = NonceIdentity | NonceHash ByteArray

derive instance Generic Nonce _
derive instance Eq Nonce
derive instance Ord Nonce

instance Show Nonce where
    show = genericShow

instance AsCbor Nonce where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.Nonce -> Nonce
fromCsl = Csl.nonce_getHash >>> toMaybe >>> case _ of
  Just hash -> NonceHash hash
  Nothing -> NonceIdentity

toCsl :: Nonce -> Csl.Nonce
toCsl = case _ of
  NonceIdentity -> Csl.nonce_newIdentity
  NonceHash hash -> Csl.nonce_newFromHash hash

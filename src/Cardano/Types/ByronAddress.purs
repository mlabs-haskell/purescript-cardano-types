module Cardano.Types.ByronAddress
  ( ByronAddress(ByronAddress)
  , fromBase58
  , toBase58
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (byronAddress_fromBase58, byronAddress_toBase58, fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Base58String (Base58String)
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd, showFromBytes)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)

newtype ByronAddress = ByronAddress Csl.ByronAddress

derive instance Generic ByronAddress _
derive instance Newtype ByronAddress _

instance Eq ByronAddress where
  eq = eqOrd

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord ByronAddress where
  compare = compareViaCslBytes `on` unwrap

instance Show ByronAddress where
  show = unwrap >>> showFromBytes "ByronAddress"

instance AsCbor ByronAddress where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson ByronAddress
derive newtype instance DecodeAeson ByronAddress

instance Arbitrary ByronAddress where
  arbitrary = pure $ unsafePartial $ fromJust $ fromBase58
    "37btjrVyb4KDXBNC4haBVPCrro8AQPHwvCMp3RFhhSVWwfFmZ6wwzSK6JK1hY6wHNmtrpTf1kdbva8TCneM2YsiXT7mrzT21EacHnPpz5YyUdj64na"

fromBase58 :: Base58String -> Maybe ByronAddress
fromBase58 = map wrap <<< toMaybe <<< byronAddress_fromBase58

toBase58 :: ByronAddress -> Base58String
toBase58 = unwrap >>> byronAddress_toBase58

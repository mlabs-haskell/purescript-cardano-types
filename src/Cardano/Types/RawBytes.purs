-- | A wrapper over `ByteArray` to indicate a byte array with no further specified meaning
module Cardano.Types.RawBytes
  ( RawBytes(RawBytes)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToMetadata (class ToMetadata)
import Data.ByteArray (ByteArray)
import Data.ByteArray as BytesArray
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | An array of Bytes with no information about the content format
newtype RawBytes = RawBytes ByteArray

instance Show RawBytes where
  show = genericShow

derive instance Newtype RawBytes _
derive instance Generic RawBytes _

derive newtype instance Eq RawBytes
derive newtype instance Ord RawBytes
derive newtype instance Semigroup RawBytes
derive newtype instance Monoid RawBytes
derive newtype instance EncodeAeson RawBytes
derive newtype instance DecodeAeson RawBytes
derive newtype instance Arbitrary RawBytes
derive newtype instance ToMetadata RawBytes
derive newtype instance FromMetadata RawBytes

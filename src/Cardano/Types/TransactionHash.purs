module Cardano.Types.TransactionHash
  ( TransactionHash(TransactionHash)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.BigNum (zero) as BigNum
import Cardano.Types.Internal.Helpers (compareViaCdlBytes, eqOrd, showFromBytes)
import Cardano.Types.PlutusData (PlutusData(Constr))
import Data.ByteArray (byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Coarbitrary, coarbitrary)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

-- | 32-bytes blake2b256 hash of a tx body.
-- | NOTE. Plutus docs might incorrectly state that it uses
-- |       SHA256 for this purposes.
newtype TransactionHash = TransactionHash Cdl.TransactionHash

derive instance Generic TransactionHash _
derive instance Newtype TransactionHash _

instance Eq TransactionHash where
  eq = eqOrd

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord TransactionHash where
  compare = compareViaCdlBytes `on` unwrap

instance Show TransactionHash where
  show = unwrap >>> showFromBytes "TransactionHash"

instance Arbitrary TransactionHash where
  arbitrary = unsafePartial $
    wrap <<< fromJust <<< fromBytes <<< byteArrayFromIntArrayUnsafe <$> vectorOf
      32
      (chooseInt 0 255)

instance Coarbitrary TransactionHash where
  coarbitrary (TransactionHash th) generator = coarbitrary (toBytes th)
    generator

-- Plutus actually has this as a zero indexed record
instance FromData TransactionHash where
  fromData (Constr n [ bytes ]) | n == BigNum.zero = TransactionHash <$>
    (fromBytes =<< fromData bytes)
  fromData _ = Nothing

-- Plutus actually has this as a zero indexed record
instance ToData TransactionHash where
  toData (TransactionHash th) = Constr BigNum.zero [ toData $ toBytes th ]

instance AsCbor TransactionHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson TransactionHash
derive newtype instance DecodeAeson TransactionHash

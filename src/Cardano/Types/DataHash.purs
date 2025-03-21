module Cardano.Types.DataHash
  ( DataHash(DataHash)
  , hashPlutusData
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Internal.Helpers (compareViaCdlBytes, eqOrd, showFromCbor)
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.PlutusData as PlutusData
import Data.ByteArray (byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype DataHash = DataHash Cdl.DataHash

derive instance Generic DataHash _
derive instance Newtype DataHash _

instance Eq DataHash where
  eq = eqOrd

instance AsCbor DataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson DataHash
derive newtype instance DecodeAeson DataHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord DataHash where
  compare = compareViaCdlBytes `on` unwrap

instance Show DataHash where
  show = unwrap >>> showFromCbor "DataHash"

-- Plutus actually has this as a zero indexed record
instance FromData DataHash where
  fromData (Constr n [ bytes ]) | n == BigNum.zero = DataHash <$>
    (fromBytes =<< fromData bytes)
  fromData _ = Nothing

-- Plutus actually has this as a zero indexed record
instance ToData DataHash where
  toData (DataHash th) = Constr BigNum.zero [ toData $ toBytes th ]

instance Arbitrary DataHash where
  arbitrary = unsafePartial $
    wrap <<< fromJust <<< fromBytes <<< byteArrayFromIntArrayUnsafe <$> vectorOf
      32
      (chooseInt 0 255)

instance Coarbitrary DataHash where
  coarbitrary (DataHash th) generator = coarbitrary (toBytes th)
    generator

hashPlutusData :: PlutusData -> DataHash
hashPlutusData = PlutusData.toCdl >>> Cdl.hashPlutusData >>> wrap

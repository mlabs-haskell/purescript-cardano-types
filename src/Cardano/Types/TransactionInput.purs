module Cardano.Types.TransactionInput
  ( TransactionInput(TransactionInput)
  , fromCsl
  , toCsl
  , _transactionId
  , _index
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.FromData (class FromData, fromData)
import Cardano.Serialization.Lib
  ( transactionInput_index
  , transactionInput_new
  , transactionInput_transactionId
  )
import Cardano.Serialization.Lib as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.Types.BigNum (zero) as BigNum
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.TransactionHash (TransactionHash(TransactionHash))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.UInt (UInt, toInt)
import Data.UInt as UInt
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (chooseInt)
import Type.Proxy (Proxy(Proxy))

newtype TransactionInput = TransactionInput
  { transactionId :: TransactionHash
  , index :: UInt
  }

derive instance Newtype TransactionInput _
derive instance Generic TransactionInput _
derive newtype instance Eq TransactionInput
derive newtype instance EncodeAeson TransactionInput
derive newtype instance DecodeAeson TransactionInput

-- Potential fix me: the below is based on a small sample of smart contract
-- transactions, so fix this as required.
-- Not newtype derived this because it is not lexicographical as `index` is tested
-- before `transactionId`. We require lexicographical order over hexstring
-- `TransactionHash`, then `index`, seemingly inline with Cardano/Plutus.
instance Ord TransactionInput where
  compare (TransactionInput txInput) (TransactionInput txInput') =
    case compare txInput.transactionId txInput'.transactionId of
      EQ -> compare txInput.index txInput'.index
      x -> x

instance Show TransactionInput where
  show (TransactionInput { transactionId, index }) = -- fixup unlawful UInt instance

    "(TransactionInput { transactionId: " <> show transactionId <> ", index: UInt.fromInt " <> show (UInt.toInt index)
      <> " })"

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance FromData TransactionInput where
  fromData (Constr n [ txId, idx ]) | n == BigNum.zero =
    TransactionInput <$>
      ({ transactionId: _, index: _ } <$> fromData txId <*> fromData idx)
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance ToData TransactionInput where
  toData (TransactionInput { transactionId, index }) =
    Constr BigNum.zero [ toData transactionId, toData index ]

instance Arbitrary TransactionInput where
  arbitrary = TransactionInput <$>
    ( { transactionId: _
      , index: _
      } <$> arbitrary <*> (UInt.fromInt <$> chooseInt 0 1000)
    )

instance Coarbitrary TransactionInput where
  coarbitrary (TransactionInput input) generator =
    coarbitrary (toInt input.index) $ coarbitrary input.transactionId generator

instance AsCbor TransactionInput where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.TransactionInput -> TransactionInput
fromCsl input =
  let
    index = UInt.fromNumber $ transactionInput_index input
    transactionId = TransactionHash $ transactionInput_transactionId input
  in
    TransactionInput
      { transactionId
      , index
      }

toCsl :: TransactionInput -> Csl.TransactionInput
toCsl (TransactionInput { transactionId, index }) = do
  transactionInput_new (unwrap transactionId) (UInt.toNumber index)

_transactionId :: Lens' TransactionInput TransactionHash
_transactionId = _Newtype <<< prop (Proxy :: Proxy "transactionId")

_index :: Lens' TransactionInput UInt
_index = _Newtype <<< prop (Proxy :: Proxy "index")

module Cardano.Types.Transaction
  ( Transaction(Transaction)
  , empty
  , toCsl
  , fromCsl
  , hash
  , _body
  , _isValid
  , _witnessSet
  , _auxiliaryData
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (transaction_setIsValid)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Cardano.Types.AuxiliaryData as AuxiliaryData
import Cardano.Types.TransactionBody (TransactionBody)
import Cardano.Types.TransactionBody as TransactionBody
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.TransactionWitnessSet (TransactionWitnessSet)
import Cardano.Types.TransactionWitnessSet as TransactionWitnessSet
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Effect.Unsafe (unsafePerformEffect)
import Literals.Undefined (undefined)
import Type.Proxy (Proxy(Proxy))
import Unsafe.Coerce (unsafeCoerce)

newtype Transaction = Transaction
  { body :: TransactionBody
  , witnessSet :: TransactionWitnessSet
  , auxiliaryData :: Maybe AuxiliaryData
  , isValid :: Boolean
  }

derive instance Newtype Transaction _
derive instance Eq Transaction
derive instance Generic Transaction _
derive instance Ord Transaction

instance Show Transaction where
  show = genericShow

instance AsCbor Transaction where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

derive newtype instance EncodeAeson Transaction
derive newtype instance DecodeAeson Transaction

empty :: Transaction
empty = Transaction
  { body: TransactionBody.empty
  , witnessSet: mempty
  , auxiliaryData: mempty
  , isValid: true
  }

hash :: Transaction -> TransactionHash
hash = unwrap
  >>> _.body
  >>> TransactionBody.toCsl
  >>> Csl.hashTransaction
  >>> wrap

fromCsl :: Csl.Transaction -> Transaction
fromCsl tx =
  let
    body = TransactionBody.fromCsl $ Csl.transaction_body tx
    witnessSet = TransactionWitnessSet.fromCsl $ Csl.transaction_witnessSet tx
    auxiliaryData = map AuxiliaryData.fromCsl $ toMaybe $ Csl.transaction_auxiliaryData tx
    isValid = Csl.transaction_isValid tx
  in
    Transaction { body, witnessSet, auxiliaryData, isValid }

toCsl :: Transaction -> Csl.Transaction
toCsl (Transaction { body, witnessSet, auxiliaryData, isValid }) = do
  unsafePerformEffect do
    let
      tx = Csl.transaction_new (TransactionBody.toCsl body)
        (TransactionWitnessSet.toCsl witnessSet)
        -- TODO: fix partiality here
        (fromMaybe (unsafeCoerce undefined) (AuxiliaryData.toCsl <$> auxiliaryData))
    transaction_setIsValid tx isValid
    pure tx

_body :: Lens' Transaction TransactionBody
_body = _Newtype <<< prop (Proxy :: Proxy "body")

_isValid :: Lens' Transaction Boolean
_isValid = _Newtype <<< prop (Proxy :: Proxy "isValid")

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = _Newtype <<< prop (Proxy :: Proxy "witnessSet")

_auxiliaryData :: Lens' Transaction (Maybe AuxiliaryData)
_auxiliaryData = _Newtype <<< prop (Proxy :: Proxy "auxiliaryData")

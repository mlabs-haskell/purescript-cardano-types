module Cardano.Types.Transaction
  ( Transaction(Transaction)
  , empty
  , toCsl
  , fromCsl
  , hashTransaction
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
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Effect.Unsafe (unsafePerformEffect)

newtype Transaction = Transaction
  { body :: TransactionBody
  , witnessSet :: TransactionWitnessSet
  , auxiliaryData :: AuxiliaryData
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

hashTransaction :: Transaction -> TransactionHash
hashTransaction = unwrap
  >>> _.body
  >>> TransactionBody.toCsl
  >>> Csl.hashTransaction
  >>> wrap

fromCsl :: Csl.Transaction -> Transaction
fromCsl tx =
  let
    body = TransactionBody.fromCsl $ Csl.transaction_body tx
    witnessSet = TransactionWitnessSet.fromCsl $ Csl.transaction_witnessSet tx
    auxiliaryData = fromMaybe mempty $ map AuxiliaryData.fromCsl $ toMaybe $ Csl.transaction_auxiliaryData tx
    isValid = Csl.transaction_isValid tx
  in
    Transaction { body, witnessSet, auxiliaryData, isValid }

toCsl :: Transaction -> Csl.Transaction
toCsl (Transaction { body, witnessSet, auxiliaryData, isValid }) = do
  unsafePerformEffect do
    let
      tx = Csl.transaction_new (TransactionBody.toCsl body)
        (TransactionWitnessSet.toCsl witnessSet)
        (AuxiliaryData.toCsl auxiliaryData)
    transaction_setIsValid tx isValid
    pure tx

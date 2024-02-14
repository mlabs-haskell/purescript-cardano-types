module Cardano.Types.Transaction
  ( Transaction(Transaction)
  , toCsl
  , fromCsl
  , hashTransaction
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Cardano.Types.AuxiliaryData as AuxiliaryData
import Cardano.Types.TransactionBody (TransactionBody)
import Cardano.Types.TransactionBody as TransactionBody
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.TransactionWitnessSet (TransactionWitnessSet)
import Cardano.Types.TransactionWitnessSet as TransactionWitnessSet
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function (($), (>>>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype Transaction = Transaction
  { body :: TransactionBody
  , witnessSet :: TransactionWitnessSet
  , auxiliaryData :: AuxiliaryData
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
  in
    Transaction { body, witnessSet, auxiliaryData }

toCsl :: Transaction -> Csl.Transaction
toCsl (Transaction { body, witnessSet, auxiliaryData }) =
  Csl.transaction_new (TransactionBody.toCsl body)
    (TransactionWitnessSet.toCsl witnessSet)
    (AuxiliaryData.toCsl auxiliaryData)

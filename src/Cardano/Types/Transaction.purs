module Cardano.Types.Transaction
  ( Transaction(Transaction)
  , empty
  , toCdl
  , fromCdl
  , hash
  , _body
  , _isValid
  , _witnessSet
  , _auxiliaryData
  , findUtxos
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (transaction_setIsValid)
import Cardano.Data.Lite as Cdl
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Cardano.Types.AuxiliaryData as AuxiliaryData
import Cardano.Types.TransactionBody (TransactionBody, _outputs)
import Cardano.Types.TransactionBody as TransactionBody
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput))
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput), toUtxoMap)
import Cardano.Types.TransactionWitnessSet (TransactionWitnessSet)
import Cardano.Types.TransactionWitnessSet as TransactionWitnessSet
import Cardano.Types.UtxoMap (UtxoMap)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt
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
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

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
  >>> TransactionBody.toCdl
  >>> Cdl.hashTransaction
  >>> wrap

findUtxos
  :: (TransactionOutput -> Boolean)
  -> Transaction
  -> UtxoMap
findUtxos pred tx = toUtxoMap $
  Array.filter pred (tx ^. _body <<< _outputs) #
    mapWithIndex \ix output -> TransactionUnspentOutput
      { input: TransactionInput { transactionId, index: UInt.fromInt ix }
      , output
      }
  where
  transactionId = hash tx

fromCdl :: Cdl.Transaction -> Transaction
fromCdl tx =
  let
    body = TransactionBody.fromCdl $ Cdl.transaction_body tx
    witnessSet = TransactionWitnessSet.fromCdl $ Cdl.transaction_witnessSet tx
    auxiliaryData = map AuxiliaryData.fromCdl $ toMaybe $ Cdl.transaction_auxiliaryData tx
    isValid = Cdl.transaction_isValid tx
  in
    Transaction { body, witnessSet, auxiliaryData, isValid }

toCdl :: Transaction -> Cdl.Transaction
toCdl (Transaction { body, witnessSet, auxiliaryData, isValid }) = do
  unsafePerformEffect do
    let
      tx = Cdl.transaction_new (TransactionBody.toCdl body)
        (TransactionWitnessSet.toCdl witnessSet)
        -- TODO: fix partiality here
        (fromMaybe (unsafeCoerce undefined) (AuxiliaryData.toCdl <$> auxiliaryData))
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

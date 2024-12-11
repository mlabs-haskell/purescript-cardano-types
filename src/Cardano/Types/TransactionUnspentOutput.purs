module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , toUtxoMap
  , fromUtxoMap
  , fromCsl
  , toCsl
  , _input
  , _output
  , filterUtxos
  , hasTransactionHash
  , pprintTransactionUnspentOutput
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( transactionUnspentOutput_input
  , transactionUnspentOutput_new
  , transactionUnspentOutput_output
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.TransactionInput
  ( TransactionInput(TransactionInput)
  , pprintTransactionInput
  )
import Cardano.Types.TransactionInput as TransactionInput
import Cardano.Types.TransactionOutput
  ( TransactionOutput
  , pprintTransactionOutput
  )
import Cardano.Types.TransactionOutput as TransactionOutput
import Cardano.Types.UtxoMap (UtxoMap)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Log.Tag (TagSet, tagSetTag)
import Data.Log.Tag as Tag
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Type.Proxy (Proxy(Proxy))

newtype TransactionUnspentOutput = TransactionUnspentOutput
  { input :: TransactionInput
  , output :: TransactionOutput
  }

derive instance Generic TransactionUnspentOutput _
derive instance Newtype TransactionUnspentOutput _
derive newtype instance Eq TransactionUnspentOutput
derive newtype instance Ord TransactionUnspentOutput
derive newtype instance EncodeAeson TransactionUnspentOutput
derive newtype instance DecodeAeson TransactionUnspentOutput

instance Show TransactionUnspentOutput where
  show = genericShow

instance AsCbor TransactionUnspentOutput where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
toUtxoMap = Map.fromFoldable <<< map
  \(TransactionUnspentOutput { input, output }) -> Tuple input output

fromUtxoMap :: UtxoMap -> Array TransactionUnspentOutput
fromUtxoMap =
  Map.toUnfoldable >>>
    map \(Tuple input output) ->
      TransactionUnspentOutput { input, output }

filterUtxos :: (TransactionUnspentOutput -> Boolean) -> UtxoMap -> UtxoMap
filterUtxos f =
  fromUtxoMap >>> Array.filter f >>> toUtxoMap

hasTransactionHash :: TransactionHash -> TransactionUnspentOutput -> Boolean
hasTransactionHash
  hash
  (TransactionUnspentOutput { input: TransactionInput { transactionId } }) =
  hash == transactionId

fromCsl
  :: Csl.TransactionUnspentOutput -> TransactionUnspentOutput
fromCsl tuo = do
  let
    input = TransactionInput.fromCsl $ transactionUnspentOutput_input tuo
    output = TransactionOutput.fromCsl $ transactionUnspentOutput_output tuo
  TransactionUnspentOutput { input, output }

toCsl :: TransactionUnspentOutput -> Csl.TransactionUnspentOutput
toCsl (TransactionUnspentOutput { input, output }) =
  transactionUnspentOutput_new (TransactionInput.toCsl input) (TransactionOutput.toCsl output)

_output :: Lens' TransactionUnspentOutput TransactionOutput
_output = _Newtype <<< prop (Proxy :: Proxy "output")

_input :: Lens' TransactionUnspentOutput TransactionInput
_input = _Newtype <<< prop (Proxy :: Proxy "input")

pprintTransactionUnspentOutput :: TransactionUnspentOutput -> TagSet
pprintTransactionUnspentOutput (TransactionUnspentOutput { input, output }) =
  Tag.fromArray $
    [ "input" `tagSetTag` pprintTransactionInput input
    , "output" `tagSetTag` pprintTransactionOutput output
    ]

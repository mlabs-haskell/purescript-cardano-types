module Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , toUtxoMap
  , fromUtxoMap
  , fromCsl
  , toCsl
  , _input
  , _output
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( transactionUnspentOutput_input
  , transactionUnspentOutput_new
  , transactionUnspentOutput_output
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionOutput as TransactionOutput
import Cardano.Types.UtxoMap (UtxoMap)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
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

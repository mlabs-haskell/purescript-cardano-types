module Cardano.Types.TransactionOutput where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Serialization.Lib
  ( transactionOutput_address
  , transactionOutput_amount
  , transactionOutput_dataHash
  , transactionOutput_new
  , transactionOutput_plutusData
  , transactionOutput_scriptRef
  , transactionOutput_setDataHash
  , transactionOutput_setPlutusData
  , transactionOutput_setScriptRef
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Cardano.Types.MultiAsset (MultiAsset(MultiAsset))
import Cardano.Types.OutputDatum
  ( OutputDatum(OutputDatum, OutputDatumHash)
  , pprintOutputDatum
  )
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.ScriptRef (ScriptRef)
import Cardano.Types.ScriptRef as ScriptRef
import Cardano.Types.Value (Value(Value), pprintValue)
import Cardano.Types.Value as Value
import Control.Alt ((<|>))
import Data.ByteArray (byteArrayToHex)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: Maybe OutputDatum
  , scriptRef :: Maybe ScriptRef
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

-- We want to have an Ord instance for TransactionOutput, but we don't
-- want to provide Ord instances for Value and MultiAsset to the users
-- to prevent confusion.
instance Ord TransactionOutput where
  compare = compare `on` unTransactionOutput
    where
    unTransactionOutput (TransactionOutput rec) = rec
      { amount =
          unValue rec.amount
      }
    unValue (Value c ma) = c /\ unMultiAsset ma
    unMultiAsset (MultiAsset ma) = ma

derive newtype instance EncodeAeson TransactionOutput
derive newtype instance DecodeAeson TransactionOutput

instance Show TransactionOutput where
  show = genericShow

instance AsCbor TransactionOutput where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

pprintTransactionOutput :: TransactionOutput -> TagSet
pprintTransactionOutput
  (TransactionOutput { address, amount, datum, scriptRef }) =
  TagSet.fromArray $
    [ "address" `tag` show address
    , "amount" `tagSetTag` pprintValue amount
    ] <> outputDatumTagSet <> referenceScriptTagSet
  where
  outputDatumTagSet = maybe [] (pure <<< pprintOutputDatum) datum
  referenceScriptTagSet = maybe []
    (pure <<< tag "referenceScript" <<< byteArrayToHex <<< unwrap <<< encodeCbor)
    scriptRef

fromCsl :: Csl.TransactionOutput -> TransactionOutput
fromCsl to =
  TransactionOutput { address, amount, datum, scriptRef }
  where
  address = Address.fromCsl $ transactionOutput_address to
  amount = Value.fromCsl $ transactionOutput_amount to
  datum = (OutputDatum <<< PlutusData.fromCsl <$> toMaybe (transactionOutput_plutusData to)) <|>
    (OutputDatumHash <<< wrap <$> toMaybe (transactionOutput_dataHash to))
  scriptRef = ScriptRef.fromCsl <$> toMaybe (transactionOutput_scriptRef to)

toCsl :: TransactionOutput -> Csl.TransactionOutput
toCsl (TransactionOutput { address, amount, datum, scriptRef }) = unsafePerformEffect do
  let cslOutput = transactionOutput_new (Address.toCsl address) (Value.toCsl amount)
  for_ datum case _ of
    OutputDatumHash dh -> transactionOutput_setDataHash cslOutput $ unwrap dh
    OutputDatum dt -> transactionOutput_setPlutusData cslOutput $ PlutusData.toCsl dt
  for_ scriptRef $ transactionOutput_setScriptRef cslOutput <<< ScriptRef.toCsl
  pure cslOutput

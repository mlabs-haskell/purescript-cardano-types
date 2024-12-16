module Cardano.Types.TransactionOutput
  ( TransactionOutput(TransactionOutput)
  , pprintTransactionOutput
  , fromCsl
  , toCsl
  , minAdaForOutput
  , _amount
  , _scriptRef
  , _datum
  , _address
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Data.Lite
  ( dataCost_newCoinsPerByte
  , dataOption_asData
  , dataOption_asHash
  , dataOption_newData
  , dataOption_newHash
  , data_encodedPlutusData
  , data_new
  , data_toBytes
  , plutusData_fromBytes
  , plutusData_newBytes
  , transactionOutput_address
  , transactionOutput_amount
  , transactionOutput_datumOption
  , transactionOutput_new
  , transactionOutput_scriptRef
  , transactionOutput_setDatumOption
  , transactionOutput_setScriptRef
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Coin (Coin)
import Cardano.Types.MultiAsset (MultiAsset(MultiAsset))
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum, OutputDatumHash), pprintOutputDatum)
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
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Type.Proxy (Proxy(Proxy))

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

instance Arbitrary TransactionOutput where
  arbitrary = genericArbitrary

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

-- | Accepts a coins per byte parameter value
minAdaForOutput :: TransactionOutput -> BigNum -> Coin
minAdaForOutput output dataCost = wrap $ wrap $
  Csl.minAdaForOutput (toCsl output) (dataCost_newCoinsPerByte $ unwrap dataCost)

fromCsl :: Csl.TransactionOutput -> TransactionOutput
fromCsl to =
  TransactionOutput { address, amount, datum, scriptRef }
  where
  address = Address.fromCsl $ transactionOutput_address to
  amount = Value.fromCsl $ transactionOutput_amount to
  mDatumOption = toMaybe (transactionOutput_datumOption to)
  datum =
    ( OutputDatum <<< PlutusData.fromCsl <$>
        ( toMaybe <<< plutusData_fromBytes =<<
            ( (data_encodedPlutusData) <$>
                ( (toMaybe <<< dataOption_asData) =<<
                    mDatumOption
                )
            )
        )
    ) <|>
      (OutputDatumHash <<< wrap <$> ((toMaybe <<< dataOption_asHash) =<< mDatumOption))
  scriptRef = ScriptRef.fromCsl <$> toMaybe (transactionOutput_scriptRef to)

toCsl :: TransactionOutput -> Csl.TransactionOutput
toCsl (TransactionOutput { address, amount, datum, scriptRef }) = unsafePerformEffect do
  let cslOutput = transactionOutput_new (Address.toCsl address) (Value.toCsl amount)
  for_ datum case _ of
    OutputDatumHash dh -> transactionOutput_setDatumOption cslOutput $ (dataOption_newHash $ unwrap dh)
    OutputDatum dt -> transactionOutput_setDatumOption cslOutput $
      (dataOption_newData $ data_new $ Csl.toBytes $ PlutusData.toCsl dt)
  for_ scriptRef $ transactionOutput_setScriptRef cslOutput <<< ScriptRef.toCsl
  pure cslOutput

_amount :: Lens' TransactionOutput Value
_amount = _Newtype <<< prop (Proxy :: Proxy "amount")

_scriptRef :: Lens' TransactionOutput (Maybe ScriptRef)
_scriptRef = _Newtype <<< prop (Proxy :: Proxy "scriptRef")

_datum :: Lens' TransactionOutput (Maybe OutputDatum)
_datum = _Newtype <<< prop (Proxy :: Proxy "datum")

_address :: Lens' TransactionOutput Address
_address = _Newtype <<< prop (Proxy :: Proxy "address")

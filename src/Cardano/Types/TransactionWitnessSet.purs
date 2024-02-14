module Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( transactionWitnessSet_new
  , transactionWitnessSet_setBootstraps
  , transactionWitnessSet_setNativeScripts
  , transactionWitnessSet_setPlutusData
  , transactionWitnessSet_setPlutusScripts
  , transactionWitnessSet_setRedeemers
  , transactionWitnessSet_setVkeys
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BootstrapWitness (BootstrapWitness)
import Cardano.Types.BootstrapWitness as BootstrapWitness
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd, withNonEmptyArray)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Cardano.Types.Vkeywitness (Vkeywitness)
import Cardano.Types.Vkeywitness as Vkeywitness
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Array Vkeywitness
  , nativeScripts :: Array NativeScript
  , bootstraps :: Array BootstrapWitness
  , plutusScripts :: Array PlutusScript
  , plutusData :: Array PlutusData
  , redeemers :: Array Redeemer
  }

derive instance Newtype TransactionWitnessSet _
derive instance Generic TransactionWitnessSet _

instance Eq TransactionWitnessSet where
  eq = eqOrd

instance Ord TransactionWitnessSet where
  compare = compareViaCslBytes `on` toCsl

instance Show TransactionWitnessSet where
  show = genericShow

instance AsCbor TransactionWitnessSet where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.TransactionWitnessSet -> TransactionWitnessSet
fromCsl =
  unsafePerformEffect $ throw "TransactionWitnessSet.fromCsl: not implemented"

toCsl :: TransactionWitnessSet -> Csl.TransactionWitnessSet
toCsl
  ( TransactionWitnessSet
      { vkeys
      , nativeScripts
      , bootstraps
      , plutusScripts
      , plutusData
      , redeemers
      }
  ) = unsafePerformEffect do
  ws <- transactionWitnessSet_new
  withNonEmptyArray (Vkeywitness.toCsl <$> vkeys) $
    transactionWitnessSet_setVkeys ws
  withNonEmptyArray (NativeScript.toCsl <$> nativeScripts) $
    transactionWitnessSet_setNativeScripts ws
  withNonEmptyArray (BootstrapWitness.toCsl <$> bootstraps) $
    transactionWitnessSet_setBootstraps ws
  withNonEmptyArray (PlutusScript.toCsl <$> plutusScripts) $
    transactionWitnessSet_setPlutusScripts ws
  withNonEmptyArray (PlutusData.toCsl <$> plutusData) $
    transactionWitnessSet_setPlutusData ws
  withNonEmptyArray (Redeemer.toCsl <$> redeemers) $
    transactionWitnessSet_setRedeemers ws
  pure ws

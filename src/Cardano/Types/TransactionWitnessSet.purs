module Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  , fromCdl
  , toCdl
  , _redeemers
  , _plutusData
  , _plutusScripts
  , _nativeScripts
  , _vkeys
  , _bootstraps
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( transactionWitnessSet_bootstraps
  , transactionWitnessSet_nativeScripts
  , transactionWitnessSet_new
  , transactionWitnessSet_plutusData
  , transactionWitnessSet_plutusScripts_v1
  , transactionWitnessSet_plutusScripts_v2
  , transactionWitnessSet_plutusScripts_v3
  , transactionWitnessSet_redeemers
  , transactionWitnessSet_setBootstraps
  , transactionWitnessSet_setNativeScripts
  , transactionWitnessSet_setPlutusData
  , transactionWitnessSet_setPlutusScripts_v1
  , transactionWitnessSet_setPlutusScripts_v2
  , transactionWitnessSet_setPlutusScripts_v3
  , transactionWitnessSet_setRedeemers
  , transactionWitnessSet_setVkeys
  , transactionWitnessSet_vkeys
  , unpackListContainer
  )
import Cardano.Data.Lite as Cdl
import Cardano.Types.BootstrapWitness (BootstrapWitness)
import Cardano.Types.BootstrapWitness as BoostrapWitness
import Cardano.Types.BootstrapWitness as BootstrapWitness
import Cardano.Types.Internal.Helpers (withNonEmptyArray)
import Cardano.Types.Language (Language(PlutusV1, PlutusV2, PlutusV3))
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Cardano.Types.Vkeywitness (Vkeywitness)
import Cardano.Types.Vkeywitness as Vkeywitness
import Data.Array (nub)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy(Proxy))

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
derive newtype instance EncodeAeson TransactionWitnessSet
derive newtype instance DecodeAeson TransactionWitnessSet
derive newtype instance Semigroup TransactionWitnessSet
derive newtype instance Monoid TransactionWitnessSet
derive instance Eq TransactionWitnessSet
derive instance Ord TransactionWitnessSet

instance Show TransactionWitnessSet where
  show = genericShow

instance AsCbor TransactionWitnessSet where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.TransactionWitnessSet -> TransactionWitnessSet
fromCdl ws =
  TransactionWitnessSet
    { vkeys
    , nativeScripts
    , bootstraps
    , plutusScripts
    , plutusData
    , redeemers
    }
  where
  use :: forall a. (Cdl.TransactionWitnessSet -> Nullable a) -> Maybe a
  use f = toMaybe (f ws)

  vkeys = map Vkeywitness.fromCdl $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_vkeys
  nativeScripts = map NativeScript.fromCdl $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_nativeScripts
  bootstraps = map BoostrapWitness.fromCdl $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_bootstraps
  plutusScripts_v1 = map (flip PlutusScript.fromCdl PlutusV1) $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_plutusScripts_v1
  plutusScripts_v2 = map (flip PlutusScript.fromCdl PlutusV2) $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_plutusScripts_v2
  plutusScripts_v3 = map (flip PlutusScript.fromCdl PlutusV3) $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_plutusScripts_v3
  plutusScripts = plutusScripts_v1 <> plutusScripts_v2 <> plutusScripts_v3
  plutusData = map PlutusData.fromCdl $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_plutusData
  redeemers = map Redeemer.fromCdl $ fromMaybe []
    $ unpackListContainer <$> use transactionWitnessSet_redeemers

toCdl :: TransactionWitnessSet -> Cdl.TransactionWitnessSet
toCdl
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
  withNonEmptyArray (Vkeywitness.toCdl <$> vkeys) $
    transactionWitnessSet_setVkeys ws
  withNonEmptyArray (NativeScript.toCdl <$> nativeScripts) $
    transactionWitnessSet_setNativeScripts ws
  withNonEmptyArray (BootstrapWitness.toCdl <$> bootstraps) $
    transactionWitnessSet_setBootstraps ws
  withNonEmptyArray (PlutusScript.toCdl <$> nub ps_v1) $ transactionWitnessSet_setPlutusScripts_v1 ws
  withNonEmptyArray (PlutusScript.toCdl <$> nub ps_v2) $ transactionWitnessSet_setPlutusScripts_v2 ws
  withNonEmptyArray (PlutusScript.toCdl <$> nub ps_v3) $ transactionWitnessSet_setPlutusScripts_v3 ws
  withNonEmptyArray (PlutusData.toCdl <$> plutusData) $
    transactionWitnessSet_setPlutusData ws
  withNonEmptyArray (Redeemer.toCdl <$> redeemers) $
    transactionWitnessSet_setRedeemers ws
  pure ws
  where
  filterByLang lang = Array.filter (\(PlutusScript (_ /\ l)) -> lang == l)
  ps_v1 = filterByLang PlutusV1 plutusScripts
  ps_v2 = filterByLang PlutusV2 plutusScripts
  ps_v3 = filterByLang PlutusV3 plutusScripts

_redeemers :: Lens' TransactionWitnessSet (Array Redeemer)
_redeemers = _Newtype <<< prop (Proxy :: Proxy "redeemers")

_plutusData :: Lens' TransactionWitnessSet (Array PlutusData)
_plutusData = _Newtype <<< prop (Proxy :: Proxy "plutusData")

_plutusScripts :: Lens' TransactionWitnessSet (Array PlutusScript)
_plutusScripts = _Newtype <<< prop (Proxy :: Proxy "plutusScripts")

_nativeScripts :: Lens' TransactionWitnessSet (Array NativeScript)
_nativeScripts = _Newtype <<< prop (Proxy :: Proxy "nativeScripts")

_vkeys :: Lens' TransactionWitnessSet (Array Vkeywitness)
_vkeys = _Newtype <<< prop (Proxy :: Proxy "vkeys")

_bootstraps :: Lens' TransactionWitnessSet (Array BootstrapWitness)
_bootstraps = _Newtype <<< prop (Proxy :: Proxy "bootstraps")

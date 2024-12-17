module Cardano.Types.AuxiliaryData
  ( AuxiliaryData(AuxiliaryData)
  , fromCsl
  , toCsl
  , hashAuxiliaryData
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( auxiliaryData_metadata
  , auxiliaryData_nativeScripts
  , auxiliaryData_new
  , auxiliaryData_plutusScripts_v1
  , auxiliaryData_plutusScripts_v2
  , auxiliaryData_plutusScripts_v3
  , auxiliaryData_setMetadata
  , auxiliaryData_setNativeScripts
  , auxiliaryData_setPlutusScripts_v1
  , auxiliaryData_setPlutusScripts_v2
  , auxiliaryData_setPlutusScripts_v3
  , packListContainer
  , unpackListContainer
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.AuxiliaryDataHash (AuxiliaryDataHash)
import Cardano.Types.GeneralTransactionMetadata (GeneralTransactionMetadata)
import Cardano.Types.GeneralTransactionMetadata as GeneralTransactionMetadatum
import Cardano.Types.Internal.Helpers (withNonEmptyArray)
import Cardano.Types.Language (Language(PlutusV1, PlutusV2, PlutusV3))
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PlutusScript as PlutusScript
import Control.Apply (lift2)
import Data.Array (nub, union)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , nativeScripts :: Maybe (Array NativeScript)
  , plutusScripts :: Maybe (Array PlutusScript)
  }

derive instance Generic AuxiliaryData _
derive instance Newtype AuxiliaryData _
derive newtype instance Eq AuxiliaryData
derive newtype instance Ord AuxiliaryData
derive newtype instance EncodeAeson AuxiliaryData
derive newtype instance DecodeAeson AuxiliaryData

instance Show AuxiliaryData where
  show = genericShow

instance Semigroup AuxiliaryData where
  append (AuxiliaryData ad) (AuxiliaryData ad') =
    AuxiliaryData
      { metadata: ad.metadata <> ad'.metadata
      , nativeScripts: lift2 union ad.nativeScripts ad'.nativeScripts
      , plutusScripts: lift2 union ad.plutusScripts ad'.plutusScripts
      }

instance Monoid AuxiliaryData where
  mempty = AuxiliaryData
    { metadata: Nothing
    , nativeScripts: Nothing
    , plutusScripts: Nothing
    }

instance AsCbor AuxiliaryData where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

hashAuxiliaryData :: AuxiliaryData -> AuxiliaryDataHash
hashAuxiliaryData = toCsl >>> Csl.hashAuxiliaryData >>> wrap

toCsl :: AuxiliaryData -> Csl.AuxiliaryData
toCsl
  (AuxiliaryData { metadata, nativeScripts, plutusScripts }) = unsafePerformEffect do
  ad <- auxiliaryData_new
  for_ metadata $
    GeneralTransactionMetadatum.toCsl >>> auxiliaryData_setMetadata ad
  for_ nativeScripts $
    map NativeScript.toCsl >>> packListContainer >>> auxiliaryData_setNativeScripts ad
  withNonEmptyArray (PlutusScript.toCsl <$> nub ps_v1) $ auxiliaryData_setPlutusScripts_v1 ad
  withNonEmptyArray (PlutusScript.toCsl <$> nub ps_v2) $ auxiliaryData_setPlutusScripts_v2 ad
  withNonEmptyArray (PlutusScript.toCsl <$> nub ps_v3) $ auxiliaryData_setPlutusScripts_v3 ad
  pure ad

  where
  filterByLang lang = Array.filter (\(PlutusScript (_ /\ l)) -> lang == l)
  ps_v1 = filterByLang PlutusV1 $ fromMaybe [] plutusScripts
  ps_v2 = filterByLang PlutusV2 $ fromMaybe [] plutusScripts
  ps_v3 = filterByLang PlutusV3 $ fromMaybe [] plutusScripts

fromCsl :: Csl.AuxiliaryData -> AuxiliaryData
fromCsl csl = AuxiliaryData
  { metadata: toMaybe (auxiliaryData_metadata csl) <#> GeneralTransactionMetadatum.fromCsl
  , nativeScripts: emptyToNothing $ toMaybe (auxiliaryData_nativeScripts csl) <#> unpackListContainer >>> map
      NativeScript.fromCsl
  , plutusScripts: if (Array.null plutusScripts) then Nothing else Just plutusScripts
  }
  where

  use :: forall a. (Csl.AuxiliaryData -> Nullable a) -> Maybe a
  use f = toMaybe (f csl)

  emptyToNothing (Just []) = Nothing
  emptyToNothing x = x

  setLanguage :: Language -> PlutusScript -> PlutusScript
  setLanguage lang (PlutusScript (ba /\ _)) = PlutusScript (ba /\ lang)

  plutusScripts_v1 = map (setLanguage PlutusV1 <<< PlutusScript.fromCsl) $ fromMaybe []
    $ unpackListContainer <$> use auxiliaryData_plutusScripts_v1
  plutusScripts_v2 = map (setLanguage PlutusV2 <<< PlutusScript.fromCsl) $ fromMaybe []
    $ unpackListContainer <$> use auxiliaryData_plutusScripts_v2
  plutusScripts_v3 = map ((setLanguage PlutusV3) <<< PlutusScript.fromCsl) $ fromMaybe []
    $ unpackListContainer <$> use auxiliaryData_plutusScripts_v3
  plutusScripts = plutusScripts_v1 <> plutusScripts_v2 <> plutusScripts_v3

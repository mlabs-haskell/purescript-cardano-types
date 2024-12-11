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
  , auxiliaryData_plutusScripts
  , auxiliaryData_setMetadata
  , auxiliaryData_setNativeScripts
  , auxiliaryData_setPlutusScripts
  , packListContainer
  , unpackListContainer
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.AuxiliaryDataHash (AuxiliaryDataHash)
import Cardano.Types.GeneralTransactionMetadata (GeneralTransactionMetadata)
import Cardano.Types.GeneralTransactionMetadata as GeneralTransactionMetadatum
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Control.Apply (lift2)
import Data.Array (union)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
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
  for_ plutusScripts $
    map PlutusScript.toCsl >>> packListContainer >>> auxiliaryData_setPlutusScripts ad
  pure ad

fromCsl :: Csl.AuxiliaryData -> AuxiliaryData
fromCsl csl = AuxiliaryData
  { metadata: toMaybe (auxiliaryData_metadata csl) <#> GeneralTransactionMetadatum.fromCsl
  , nativeScripts: toMaybe (auxiliaryData_nativeScripts csl) <#> unpackListContainer >>> map NativeScript.fromCsl
  , plutusScripts: toMaybe (auxiliaryData_plutusScripts csl) <#> unpackListContainer >>> map PlutusScript.fromCsl
  }

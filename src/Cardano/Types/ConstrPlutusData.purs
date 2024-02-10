module Cardano.Crypto.Csl.Types.ConstrPlutusData
  ( ConstrPlutusData(..)
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (constrPlutusData_new)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.PlutusData (PlutusData(..))
import Cardano.Types.PlutusData as PlutusData
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function (($), (>>>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

newtype ConstrPlutusData = ConstrPlutusData { alternative :: BigNum, data :: Array PlutusData }


derive instance Newtype ConstrPlutusData _
derive instance Eq ConstrPlutusData
derive instance Generic ConstrPlutusData _


instance Show ConstrPlutusData where
    show = genericShow

instance AsCbor ConstrPlutusData where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.ConstrPlutusData -> ConstrPlutusData
fromCsl cpd = let
  alternative = wrap $ Csl.constrPlutusData_alternative cpd
  d = fromList $ PlutusData.fromCsl $ Csl.plutusData_newList $ Csl.constrPlutusData_data cpd
  in
   wrap { alternative, data: d }

   where
     fromList (List r) = r
     fromList _ = unsafePerformEffect $ throw "Invalid PlutusData type."

toCsl :: ConstrPlutusData -> Csl.ConstrPlutusData
toCsl (ConstrPlutusData {alternative, data:d}) =
  constrPlutusData_new (unwrap alternative) (unsafePartial $ fromJust $ toMaybe $ Csl.plutusData_asList $ PlutusData.toCsl (PlutusData.List d))

module Cardano.Types.DatumSource
  ( DatumSource(..)
  , toCsl
  ) where

import Cardano.Serialization.Lib as Csl
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

data DatumSource = DatumSourceDatum PlutusData | DatumSourceTxIn TransactionInput

derive instance Eq DatumSource
derive instance Ord DatumSource
derive instance Generic DatumSource _

instance Show DatumSource where
  show = genericShow

toCsl :: DatumSource -> Csl.DatumSource
toCsl = case _ of
  DatumSourceDatum x -> Csl.datumSource_new (PlutusData.toCsl x)
  DatumSourceTxIn x -> Csl.datumSource_newRefInput (TransactionInput.toCsl x)

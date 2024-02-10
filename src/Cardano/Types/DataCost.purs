module Cardano.Types.DataCost
  ( DataCost(..)
  , fromCsl
  , toCsl
  ) where

import Cardano.Types.BigNum (BigNum)
import Cardano.Serialization.Lib as Csl
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype DataCost = DataCost { coinsPerByte :: BigNum }

derive instance Newtype DataCost _
derive instance Eq DataCost
derive instance Generic DataCost _

instance Show DataCost where
    show = genericShow

fromCsl :: Csl.DataCost -> DataCost
fromCsl = Csl.dataCost_coinsPerByte >>> wrap >>> {coinsPerByte: _} >>> wrap

toCsl :: DataCost -> Csl.DataCost
toCsl (DataCost {coinsPerByte}) = Csl.dataCost_newCoinsPerByte (unwrap coinsPerByte)

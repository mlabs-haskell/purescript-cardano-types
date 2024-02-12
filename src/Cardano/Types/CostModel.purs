module Cardano.Types.CostModel where

import Prelude

import Aeson (class EncodeAeson, class DecodeAeson)
import Cardano.Types.Int as Int
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype CostModel = CostModel (Array Int.Int)

derive instance Newtype CostModel _
derive instance Generic CostModel _
derive newtype instance Eq CostModel
derive newtype instance Ord CostModel
derive newtype instance EncodeAeson CostModel
derive newtype instance DecodeAeson CostModel

instance Show CostModel where
  show = genericShow

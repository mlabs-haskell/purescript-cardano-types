module Cardano.Types.Slot (Slot(Slot)) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types.BigNum (BigNum)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Slot = Slot BigNum

derive instance Newtype Slot _
derive instance Generic Slot _
derive newtype instance Eq Slot
derive newtype instance Ord Slot
derive newtype instance DecodeAeson Slot
derive newtype instance EncodeAeson Slot
derive newtype instance FromData Slot
derive newtype instance ToData Slot
derive newtype instance AsCbor Slot

instance Show Slot where
  show = genericShow

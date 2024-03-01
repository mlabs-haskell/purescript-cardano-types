module Cardano.Types.Epoch where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)

newtype Epoch = Epoch UInt

derive instance Newtype Epoch _
derive instance Generic Epoch _
derive newtype instance Eq Epoch
derive newtype instance Ord Epoch
derive newtype instance EncodeAeson Epoch
derive newtype instance DecodeAeson Epoch

instance Show Epoch where
  show = genericShow

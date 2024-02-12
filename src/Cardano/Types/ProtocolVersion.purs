module Cardano.Types.ProtocolVersion where

import Prelude

import Aeson (class EncodeAeson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)

newtype ProtocolVersion = ProtocolVersion
  { major :: UInt
  , minor :: UInt
  }

derive instance Newtype ProtocolVersion _
derive instance Generic ProtocolVersion _
derive newtype instance Eq ProtocolVersion
derive newtype instance Ord ProtocolVersion
derive newtype instance EncodeAeson ProtocolVersion

instance Show ProtocolVersion where
  show = genericShow

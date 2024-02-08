module Cardano.Types.Pointer
  ( Pointer(..)
  , fromCsl
  , toCsl
  )
  where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Slot (Slot)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Show.Generic (genericShow)

newtype Pointer = Pointer { slot :: Slot, txIndex :: BigNum, certIndex :: BigNum }

derive instance Eq Pointer
derive instance Ord Pointer
derive instance Generic Pointer _
derive instance Newtype Pointer _

instance Show Pointer where
  show = genericShow

derive newtype instance EncodeAeson Pointer
derive newtype instance DecodeAeson Pointer

fromCsl :: Csl.Pointer -> Pointer
fromCsl p = Pointer {slot: wrap (wrap (Csl.pointer_slotBignum p)), txIndex: wrap (Csl.pointer_txIndexBignum p), certIndex: wrap (Csl.pointer_certIndexBignum p)}


toCsl :: Pointer -> Csl.Pointer
toCsl (Pointer {slot, txIndex, certIndex}) = Csl.pointer_newPointer (unwrap (unwrap slot)) (unwrap txIndex) (unwrap certIndex)

module Cardano.Types.StakeValidatorHash
  ( StakeValidatorHash(StakeValidatorHash)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(..)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype StakeValidatorHash = StakeValidatorHash ScriptHash

derive instance Generic StakeValidatorHash _
derive instance Newtype StakeValidatorHash _
derive newtype instance Eq StakeValidatorHash
derive newtype instance Ord StakeValidatorHash
derive newtype instance ToData StakeValidatorHash
derive newtype instance FromData StakeValidatorHash

instance DecodeAeson StakeValidatorHash where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getStakeValidatorHash" >=> decodeAeson >>> map StakeValidatorHash)

instance EncodeAeson StakeValidatorHash where
  encodeAeson (StakeValidatorHash hash) =
    encodeAeson { "getStakeValidatorHash": hash }

instance Show StakeValidatorHash where
  show = genericShow